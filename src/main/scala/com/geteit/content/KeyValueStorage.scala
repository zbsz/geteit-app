package com.geteit.content

import com.geteit.concurrent.Threading
import com.geteit.db.{CachedStorageSignal, CachedStorage}
import com.geteit.events._
import com.geteit.inject.{Injectable, Injector}
import com.geteit.json.{JsonDecoder, JsonEncoder}
import com.geteit.util.returning
import com.geteit.util.Log._
import com.geteit.util.LruCache
import com.geteit.model.KeyValue

class KeyValueStorage(implicit inj: Injector) extends CachedStorage[String, KeyValue] with CachedStorageSignal[String, KeyValue] with Injectable {
  private implicit val eventContext = inject[EventContext]
  override protected val cache = new LruCache[String, Option[KeyValue]](32)

  def put[T: JsonEncoder](key: String, value: T) = add(KeyValue(key, value))

  def load[T: JsonDecoder](key: String) = get(key).map(_.map(_.decode))(Threading.global)

  def pref[T: JsonEncoder : JsonDecoder](key: String, default: => T)(implicit ev: EventContext): SourceSignal[T] =
    returning(Signal[T]()) { res =>
      @volatile var ignore = false

      signal(key)(ev).map(_.decode[T]).orElse(Signal.const(default)) { v =>
        ignore = true
        res ! v
        ignore = false
      } (ev)

      res { v => if (!ignore) put(key, v) } (ev)
    }
}

object KeyValueStorage {
  private implicit val tag: LogTag = "KeyValueStorage"
}
