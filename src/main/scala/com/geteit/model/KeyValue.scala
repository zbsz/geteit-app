package com.geteit.model

import com.geteit.db.Dao
import com.geteit.json.{JsonEncoder, JsonDecoder, Json}

@Json
case class KeyValue(id: String, value: String) {

  def decode[T: JsonDecoder] = implicitly[JsonDecoder[T]].apply(value)
}

object KeyValue {

  def apply[T: JsonEncoder](key: String, value: T): KeyValue = KeyValue(key, implicitly[JsonEncoder[T]].apply(value))

  implicit object KeyValueDao extends Dao[String, KeyValue] {
    override val table = Table("KeyValues", Nil)
    override def getId(v: KeyValue) = v.id
  }
}
