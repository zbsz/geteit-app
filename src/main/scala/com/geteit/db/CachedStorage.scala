package com.geteit.db

import android.database.sqlite.SQLiteDatabase
import android.support.v4.util.LruCache
import com.geteit.concurrent.{LimitedExecutionContext, Threading}
import com.geteit.events.{Subscription, EventStream, EventContext, Signal}
import com.geteit.inject.{Injectable, Injector}
import com.geteit.util.Log._
import com.geteit.util.ThrottledProcessingQueue

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

abstract class CachedStorage[K, V](implicit val dao: Dao[K, V], inj: Injector) extends Injectable {
  protected implicit val executionContext = new LimitedExecutionContext

  protected val cache: LruCache[K, Option[V]]

  val storage: Storage = inject[Storage]

  val onAdded = EventStream[V]()
  val onRemoved = EventStream[K]()
  val onUpdated = EventStream[(V, V)]() // (prev, current)

  private var loadFuture = Future.successful(Option.empty[V])

  private val saveQueue = new ThrottledProcessingQueue[SaveCmd](500.millis, { evs =>
    val removed = new mutable.HashSet[K]
    val added = new mutable.HashMap[K, V]
    evs foreach {
      case Delete(id) =>
        removed += id
        added -= id
      case Insert(item) => added(dao.getId(item)) = item
    }
    storage { implicit db =>
      Dao.inTransaction {
        dao.deleteAll(removed)
        dao.insert(added.values)
      }
    }
  }, "AudioStorageSaveQueue")

  private def cachedOrElse(key: K, default: => Future[Option[V]]): Future[Option[V]] = Option(cache.get(key)).fold(default)(Future.successful)

  private def loadFromDb(key: K): Future[Option[V]] = storage { dao.get(key)(_) } map { value =>
    Option(cache.get(key)).getOrElse {
      cache.put(key, value)
      value
    }
  }

  def find(matcher: Matcher[V]): Future[Set[K]] = {
    val dbSearch = storage { dao.find(matcher.whereSql)(_) }
    val cached = Future { cache.snapshot.asScala.collect { case (k, Some(v)) if matcher(v) => k }.toSet }
    dbSearch.flatMap(d => cached.map(_ ++ d))
  }

  def add(item: V) = Future { addInternal(dao.getId(item), item) }

  def get(key: K): Future[Option[V]] = cachedOrElse(key, Future {
    loadFuture = loadFuture recover { case _ => () } flatMap { _ => cachedOrElse(key, loadFromDb(key)) }
    loadFuture
  }.flatMap(identity))

  def getOrCreate(key: K, creator: => V): Future[V] = get(key) map { value =>
    value.orElse(Option(cache.get(key)).flatten).getOrElse { addInternal(key, creator) }
  }

  def getAll(keys: Seq[K]): Future[Seq[Option[V]]] = {
    val cachedEntries = keys.flatMap { key => Option(cache.get(key)) map { value => (key, value) } }.toMap
    val missingKeys = keys.toSet -- cachedEntries.keys
    val loaderOfMissing = { (keys: Seq[K], db: SQLiteDatabase) => keys.flatMap(key => dao.get(key)(db).map { value => (key, value) })}

    storage.withTransaction { db => loaderOfMissing(missingKeys.toSeq, db) } map { loadedEntries =>
      val loadedMap = loadedEntries .map { case (key, value) =>
        Option(cache.get(key)).map((key, _)).getOrElse {
          cache.put(key, Some(value))
          (key, Some(value))
        }
      } .toMap

      keys map { key =>
        loadedMap.get(key).orElse(cachedEntries.get(key)).getOrElse(None)
      }
    }
  }

  def update(key: K, updater: V => V): Future[Option[V]] = get(key) map { loaded =>
    Option(cache.get(key)).getOrElse(loaded) map updateInternal(key, updater)
  }

  def updateOrCreate(key: K, updater: V => V, creator: => V): Future[V] = get(key) map { loaded =>
    val prev = loaded.orElse(Option(cache.get(key)).flatten)
    prev .map { updateInternal(key, updater) } .getOrElse { addInternal(key, creator) }
  }

  def updateOrCreateAll(updaters: K Map (Option[V] => V)): Future[Set[V]] = {
    val keys = updaters.keys.toSeq
    getAll(keys) map { values =>
      val loaded = keys.zip(values).toMap
      updaters .map { case (key, updater) =>
        loaded.get(key).flatten.orElse(Option(cache.get(key)).flatten) match {
          case Some(current) => updateInternal(key, updater compose (Some(_)))(current)
          case None => addInternal(key, updater(None))
        }
      } .toSet
    }
  }

  private def addInternal(key: K, value: V): V = {
    cache.put(key, Some(value))
    saveQueue ! Insert(value)
    onAdded ! value
    value
  }

  private def updateInternal(key: K, updater: V => V)(current: V): V = {
    val updated = updater(current)
    if (updated != current) {
      cache.put(key, Some(updated))
      saveQueue ! Insert(updated)
      onUpdated ! ((current, updated))
    }
    updated
  }

  def remove(key: K): Unit = {
    cache.put(key, None)
    saveQueue ! Delete(key)
    onRemoved ! key //FIXME: should we call this? It is possible that this key was already remove (or not present at all), should we report removal then?
  }


  sealed trait SaveCmd
  case class Insert(item: V) extends SaveCmd
  case class Delete(key: K) extends SaveCmd
}


trait CachedStorageSignal[K, V] { self: CachedStorage[K, V] =>
  import Threading.global
  private implicit val tag: LogTag = "CachedStorageSignal"

  def signal(id: K)(implicit ev: EventContext): Signal[V] = new Signal[V]() {

    def reload() = self.get(id).map { _.foreach(this.publish) }

    private var observers = Seq.empty[Subscription]

    override protected def onWire(): Unit = {
      observers = Seq(
        onAdded.filter(v => dao.getId(v) == id) { publish }, // FIXME: possible race condition with reload result
        onUpdated.filter(p => dao.getId(p._2) == id) { case (_, v) => publish(v) },
        onRemoved.filter(_ == id) { _ => reload() }
      )
      reload()
    }

    override protected def onUnwire(): Unit = {
      observers foreach (_.destroy())
      value = None
    }
  }

  def findSignal(matcher: Matcher[V])(implicit ev: EventContext): Signal[Set[K]] = new Signal[Set[K]] {

    def reload(): Unit = {
      verbose(s"reload ${matcher.whereSql}")
      storage { dao.find(matcher.whereSql)(_) } onSuccess {
        case ids =>
          verbose(s"found: $ids")
          publish(value.getOrElse(Set.empty) ++ ids)
      }
    }

    private var observers = Seq.empty[Subscription]

    override protected def onWire(): Unit = {
      value = None
      observers = Seq(
        // FIXME: possible race conditions - on every upate
        onAdded { v =>
          if (matcher(v)) publish(value.getOrElse(Set.empty) + dao.getId(v))
        },
        onUpdated {
          case (prev, up) =>
            (matcher(prev), matcher(up)) match {
              case (true, false) => value foreach { s => this.publish(s - dao.getId(prev)) }
              case (false, true) => publish(value.getOrElse(Set.empty) + dao.getId(up))
              case _ =>
            }
        },
        onRemoved { id => value foreach { s => publish(s - id) } }
      )
      reload()
    }

    override protected def onUnwire(): Unit = {
      verbose(s"onUnwire")
      observers foreach (_.destroy())
      value = None
    }
  }
}