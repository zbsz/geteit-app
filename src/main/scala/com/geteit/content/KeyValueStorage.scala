package com.geteit.content

import com.geteit.concurrent.Threading
import com.geteit.db.{CachedStorageSignal, CachedStorage}
import com.geteit.events._
import com.geteit.inject.{Injectable, Injector}
import com.geteit.json.{JsonDecoder, JsonEncoder}
import com.geteit.util._
import com.geteit.util.Log._
import com.geteit.model.KeyValue

import scala.concurrent.Future

class KeyValueStorage(implicit inj: Injector) extends CachedStorage[String, KeyValue] with CachedStorageSignal[String, KeyValue] with Injectable {
  import KeyValueStorage._
  private implicit val eventContext = inject[EventContext]
  override protected val cache = new LruCache[String, Option[KeyValue]](32)

  def put[T: JsonEncoder](key: String, value: T) = add(KeyValue(key, value))

  def load[T: JsonDecoder](key: String) = get(key).map(_.map(_.decode))(Threading.global)

  def pref[T: JsonEncoder : JsonDecoder](key: String, default: => T)(implicit ev: EventContext): Signal[T] with Source[T] = {
    // set default value if doesn't exist
    load[T](key) .flatMap {
      case None => put(key, default)
      case Some(_) => Future.successful(())
    } .recoverWithLog()

    val source = signal(key)(ev).map {
      case Some(kv) => LoggedTry(kv.decode[T]).getOrElse(default)
      case None => default
    }

    new ProxySignal[T](source) with Source[T] {
      override protected def computeValue(current: Option[T]) = source.currentValue
      onChanged { v => put(key, v) }(ev)
    }
  }
}

object KeyValueStorage {
  private implicit val tag: LogTag = "KeyValueStorage"
}
