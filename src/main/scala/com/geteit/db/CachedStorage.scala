package com.geteit.db

import android.database.sqlite.SQLiteDatabase
import android.database.{Cursor, CursorWrapper}
import android.support.v4.util.LruCache
import com.geteit.concurrent.{LimitedExecutionContext, Threading}
import com.geteit.db.CachedStorageSignal.{Add, Cmd, Del}
import com.geteit.events._
import com.geteit.inject.{Injectable, Injector}
import com.geteit.util.Log._
import com.geteit.util.returning

import scala.collection.JavaConverters._
import scala.collection.breakOut
import scala.concurrent.Future
import scala.util.Try

abstract class CachedStorage[K, V](implicit val dao: Dao[K, V], inj: Injector) extends Injectable {
  private implicit val tag: LogTag = "CachedStorage"
  protected implicit val executionContext = new LimitedExecutionContext

  protected val cache: LruCache[K, Option[V]]

  val storage: Storage = inject[Storage]

  val onAdded = EventStream[Seq[V]]()
  val onRemoved = EventStream[Seq[K]]()
  val onUpdated = EventStream[Seq[(V, V)]]() // (prev, current)

  private var loadFuture = Future.successful(Option.empty[V])

  private def cachedOrElse(key: K, default: => Future[Option[V]]): Future[Option[V]] = Option(cache.get(key)).fold(default)(Future.successful)

  private def loadFromDb(key: K): Future[Option[V]] = storage.read { dao.get(key)(_) } map { value =>
    Option(cache.get(key)).getOrElse {
      cache.put(key, value)
      value
    }
  }

  def find(matcher: Matcher[V]): Future[Set[K]] = {
    val dbSearch = storage.read { dao.find(matcher.whereSql)(_) }
    val cached = Future { cache.snapshot.asScala.collect { case (k, Some(v)) if matcher(v) => k }.toSet }
    dbSearch.flatMap(d => cached.map(_ ++ d))
  }

  def query(matcher: Matcher[V]): Future[Cursor] = storage.read { dao.query(matcher.whereSql)(_) }

  def remove(matcher: Matcher[V]): Future[Unit] = find(matcher) map { _ foreach remove }

  def insert(item: V) = updateOrCreate(dao.getId(item), _ => item, item)

  def add(item: V) = addInternal(dao.getId(item), item)

  def get(key: K): Future[Option[V]] = cachedOrElse(key, Future {
    loadFuture = loadFuture recover { case t: Throwable => error("loadFuture failed", t) } flatMap { _ => cachedOrElse(key, loadFromDb(key)) }
    loadFuture
  }.flatMap(identity))

  def getOrCreate(key: K, creator: => V): Future[V] = get(key) flatMap { value =>
    value.orElse(Option(cache.get(key)).flatten).fold(addInternal(key, creator))(Future.successful)
  }

  def getAll = storage.read { dao.find("1 = 1")(_) } flatMap { ids => Future.traverse(ids)(get) } map { _.flatten }

  def getAll(keys: Seq[K]): Future[Seq[Option[V]]] = {
    val cachedEntries = keys.flatMap { key => Option(cache.get(key)) map { value => (key, value) } }.toMap
    val missingKeys = keys.toSet -- cachedEntries.keys
    val loaderOfMissing = { (keys: Seq[K], db: SQLiteDatabase) => keys.flatMap(key => dao.get(key)(db).map { value => (key, value) })}

    storage.read { db => loaderOfMissing(missingKeys.toSeq, db) } map { loadedEntries =>
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

  def update(key: K, updater: V => V): Future[Option[(V, V)]] = get(key) flatMap { loaded =>
    val prev = Option(cache.get(key)).getOrElse(loaded)
    prev.fold(Future successful Option.empty[(V, V)]) { updateInternal(key, updater)(_) }
  }

  def updateOrCreate(key: K, updater: V => V, creator: => V): Future[V] = get(key) flatMap { loaded =>
    val prev = Option(cache.get(key)).getOrElse(loaded)
    prev.fold { addInternal(key, creator) } { v => updateInternal(key, updater)(v).map(_.fold(v)(_._2)) }
  }

  def updateAll(updaters: scala.collection.Map[K, V => V]): Future[Seq[(V, V)]] = {
    val keys = updaters.keys.toSeq
    getAll(keys) flatMap { values =>
      val updated = keys.zip(values) flatMap { case (k, v) =>
        Option(cache.get(k)).flatten.orElse(v).flatMap { value =>
          val updated = updaters(k)(value)
          if (updated != value) {
            cache.put(k, Some(updated))
            Some(value -> updated)
          } else None
        }
      }

      if (updated.isEmpty) Future.successful(Seq.empty)
      else
        returning (storage { dao.insert(updated.map(_._2))(_) } .map { _ => updated }) { _ =>
          onUpdated ! updated
        }
    }
  }

  def updateOrCreateAll(updaters: K Map (Option[V] => V)): Future[Set[V]] = {
    val keys = updaters.keys.toSeq
    getAll(keys) flatMap { values =>
      val loaded: Map[K, Option[V]] = keys.zip(values).map { case (k, v) => k -> Option(cache.get(k)).flatten.orElse(v) } (breakOut)
      val toSave = Seq.newBuilder[V]
      val added = Seq.newBuilder[V]
      val updated = Seq.newBuilder[(V, V)]

      val result = updaters .map { case (key, updater) =>
        val current = loaded.get(key).flatten
        val next = updater(current)
        current match {
          case Some(c) if c != next =>
            cache.put(key, Some(next))
            toSave += next
            updated += (c -> next)
          case None =>
            cache.put(key, Some(next))
            toSave += next
            added += next
          case Some(_) => // unchanged, ignore
        }
        next
      } .toSet

      val addedResult = added.result
      val updatedResult = updated.result

      returning (storage { dao.insert(toSave.result)(_) } .map { _ => result }) { _ =>
        if (addedResult.nonEmpty) onAdded ! addedResult
        if (updatedResult.nonEmpty) onUpdated ! updatedResult
      }
    }
  }

  private def addInternal(key: K, value: V): Future[V] = {
    cache.put(key, Some(value)) match {
      case Some(`value`) => Future successful value
      case _ =>
        returning(storage { dao.insert(Seq(value))(_) }.map { _ => value }) { _ =>
          onAdded ! Seq(value)
        }
    }
  }

  private def updateInternal(key: K, updater: V => V)(current: V): Future[Option[(V, V)]] = {
    val updated = updater(current)
    if (updated == current) Future.successful(None)
    else {
      cache.put(key, Some(updated))
      returning(storage { dao.insert(Seq(updated))(_) } .map { _ => Some((current, updated)) }) { _ =>
        onUpdated ! Seq((current, updated))
      }
    }
  }

  def remove(key: K): Future[Int] = Future {
    cache.put(key, None)
    returning(storage { dao.delete(key)(_) }) { _ => onRemoved ! Seq(key) }
  } .flatMap(identity)

  def removeAll(keys: Seq[K]): Future[Unit] = Future {
    keys foreach { cache.put(_, None) }
    returning(storage { dao.deleteAll(keys)(_) }) { _ => onRemoved ! keys }
  } .flatMap(identity)
}


trait CachedStorageSignal[K, V] { self: CachedStorage[K, V] =>
  import Threading.global

  import collection.breakOut
  private implicit val tag: LogTag = "CachedStorageSignal"

  private lazy val onChanged = EventStream.union[Seq[Cmd[K, V]]](onAdded.map(_.map(Add(_))), onUpdated.map(_.map(p => Add(p._2))), onRemoved.map(_.map(Del(_))))

  private def loadAll: Future[Map[K, V]] = getAll map { _.map(v => dao.getId(v) -> v)(breakOut) }

  def all(implicit ev: EventContext): Signal[Map[K, V]] = new AggregatingSignal[Seq[Cmd[K, V]], Map[K, V]](onChanged, loadAll, { (values, cmds) =>
    var res = values
    cmds foreach {
      case Add(v) => res += (dao.getId(v) -> v)
      case Del(k) => res -= k
    }
    res
  })

  def signal(id: K)(implicit ev: EventContext): Signal[V] = new Signal[V]() {

    def reload() = self.get(id).map { _.foreach(this.publish) }

    private var observers = Seq.empty[Subscription]

    override protected def onWire(): Unit = {
      observers = Seq(
        onAdded.map(_.reverseIterator.find(v => dao.getId(v) == id)) { _.foreach(publish) }, // FIXME: possible race condition with reload result
        onUpdated.map(_.reverseIterator.find(p => dao.getId(p._2) == id)) { _.foreach(p => publish(p._2)) },
        onRemoved.filter(_.exists(_ == id)) { _ => reload() }
      )
      reload()
    }

    override protected def onUnwire(): Unit = {
      observers foreach (_.destroy())
      clear()
    }
  }

  def findSignal(matcher: Matcher[V])(implicit ev: EventContext): Signal[Set[K]] = new Signal[Set[K]] {

    def reload(): Unit = {
      verbose(s"reload ${matcher.whereSql}")
      storage.read { dao.find(matcher.whereSql)(_) } onSuccess {
        case ids =>
          verbose(s"found: $ids")
          publish(value.getOrElse(Set.empty) ++ ids)
      }
    }

    private var observers = Seq.empty[Subscription]

    override protected def onWire(): Unit = {
      clear()
      observers = Seq(
        // FIXME: possible race conditions - on every upate
        onAdded { vs =>
          vs.reverseIterator.find(matcher(_)) foreach { v => publish(value.getOrElse(Set.empty) + dao.getId(v)) }
        },
        onUpdated { _ foreach {
          case (prev, up) =>
            (matcher(prev), matcher(up)) match {
              case (true, false) => value foreach { s => this.publish(s - dao.getId(prev)) }
              case (false, true) => publish(value.getOrElse(Set.empty) + dao.getId(up))
              case _ =>
            }
        } },
        onRemoved { ids => value foreach { s => publish(s -- ids) } }
      )
      reload()
    }

    override protected def onUnwire(): Unit = {
      verbose(s"onUnwire")
      observers foreach (_.destroy())
      clear()
    }
  }

  def querySignal(matcher: Matcher[V]): Signal[Cursor] =
    Signal.wrap(onChanged).orElse(Signal.const(null)) flatMap { _ =>
      verbose(s"requery ${matcher.whereSql}")
      Signal.future(query(matcher)) map (new AutoCloseCursor(_))
    }
}

object CachedStorageSignal {
  trait Cmd[+K, +V]
  case class Add[A](v: A) extends Cmd[Nothing, A]
  case class Del[A](v: A) extends Cmd[A, Nothing]
}

class AutoCloseCursor(c: Cursor) extends CursorWrapper(c) {
  override def finalize(): Unit = {
    Try(c.close())
    super.finalize()
  }
}
