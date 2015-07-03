package com.geteit.cache

import java.lang.System.currentTimeMillis
import java.util.UUID

import android.database.sqlite.{SQLiteProgram, SQLiteDatabase}
import com.geteit.db.{Index, Dao, DbType, Id}
import com.geteit.json.{Json, JsonValue}
import com.geteit.util.Log

@JsonValue
case class Uid(str: String)

object Uid {
  def apply() = new Uid(UUID.randomUUID().toString)

  implicit object DbId extends Id[Uid] {
    override def decode(s: String) = Uid(s)
    override def encode(v: Uid): String = v.str
  }

  implicit object UidDbType extends DbType[Uid] with DbType.Text {
    override def literal(v: Uid): String = s"'${v.str}'"
    def bind(stmt: SQLiteProgram, pos: Int, v: Uid) = stmt.bindString(pos, v.str)
  }
}

@Json
case class CacheEntryData(
    key: String,
    data: Option[Array[Byte]] = None,
    lastUsed: Long = currentTimeMillis(),
    timeout: Long = CacheService.DefaultExpiryTime.toMillis,
    fileId: Uid = Uid()) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case CacheEntryData(k, d, lu, t, i) =>
      Log.warn("Comparing CacheEntryData with equals - can be slow", new IllegalStateException(""))("CacheEntryData")
      k == key && lu == lastUsed && t == timeout && i == fileId && d.map(_.toSeq) == data.map(_.toSeq)
    case _ => false
  }
}

object CacheEntryData {

  implicit object CacheEntryDao extends Dao[String, CacheEntryData] {

    object Indexes {
      val expires = new Index[CacheEntryData, Long]("lastUsed", { e => e.lastUsed + e.timeout })
      val hasData = new Index[CacheEntryData, Boolean]("hasData", _.data.nonEmpty)
      val fileId = new Index[CacheEntryData, Uid]("fileId", _.fileId)
    }

    override def getId(v: CacheEntryData): String = v.key
    override val table = new Table("CacheEntry", Seq(Indexes.expires, Indexes.hasData))

    def findAllWithData(implicit db: SQLiteDatabase): Seq[CacheEntryData] = {
      ??? // TODO: implement indexes
    }

    def findAllExpired(currentTime: Long)(implicit db: SQLiteDatabase): Seq[CacheEntryData] = {
      ??? // TODO: implement indexes
    }

    def deleteExpired(currentTime: Long)(implicit db: SQLiteDatabase): Unit = {
      ??? // TODO: implement indexes
    }
  }
}
