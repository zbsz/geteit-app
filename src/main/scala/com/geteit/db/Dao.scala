package com.geteit.db

import android.database.Cursor
import android.database.sqlite.{SQLiteDatabase, SQLiteStatement}
import com.geteit.json.{JsonDecoder, JsonEncoder}
import com.geteit.util.returning

trait Id[A] {
  def decode(s: String): A
  def encode(v: A): String = v.toString
}

object Id {
  implicit object StringId extends Id[String] {
    override def decode(s: String): String = s
    override def encode(v: String): String = v
  }
  implicit object LongId extends Id[Long] {
    override def decode(s: String): Long = s.toLong
  }
}

trait DbType[A] {
  val sqlName: String
}
object DbType {
  trait Text { self: DbType[_] =>
    override val sqlName = "TEXT"
  }
  trait Integer { self: DbType[_] =>
    override val sqlName = "INTEGER"
  }

  implicit object StringDbType extends DbType[String] with Text
  implicit object IntDbType extends DbType[Int] with Integer
  implicit object LongDbType extends DbType[Long] with Integer
  implicit object BoolDbType extends DbType[Boolean] with Integer
}

abstract class Dao[I: Id, A: JsonDecoder : JsonEncoder] {
  import Dao._

  private val decoder = implicitly[JsonDecoder[A]]
  private val encoder = implicitly[JsonEncoder[A]]
  private val _id = implicitly[Id[I]]

  private val IdIndex = 0
  private val DataIndex = 1

  val table: Table
  def getId(v: A): I

  case class Index[B: DbType](name: String, ext: A => B)

  case class Table(name: String, indexes: Seq[Index[_]])

  lazy val createTableSql = s"CREATE TABLE IF NOT EXISTS ${table.name} (_id TEXT PRIMARY KEY, _data TEXT)" // TODO: support indexes
  lazy val dropTableSql = s"DROP TABLE IF EXISTS ${table.name}"

  lazy val deleteSql = s"DELETE FROM ${table.name} WHERE _id = ?"
  lazy val insertOrReplaceSql = s"INSERT OR REPLACE INTO ${table.name} (_id, _data) VALUES (?, ?)" // TODO: indexes
  lazy val insertOrIgnoreSql = s"INSERT OR IGNORE INTO ${table.name} (_id, _data) VALUES (?, ?)"

  protected[db] def decode(c: Cursor) = decoder(c.getString(DataIndex))

  protected[db] def single(c: Cursor) = try {
    if (c.moveToFirst()) Some(decode(c)) else None
  } finally c.close()

  protected[db] def list(c: Cursor) = try {
    val builder = Seq.newBuilder[A]
    while (c.moveToNext()) builder += decode(c)
    builder.result()
  } finally c.close()

  def get(id: I)(implicit db: SQLiteDatabase): Option[A] =
    single(db.query(table.name, null, "_id = ?", Array(_id.encode(id)), null, null, null))

  def list(implicit db: SQLiteDatabase): Seq[A] = list(db.query(table.name, null, null, null, null, null, null))

  def insert(item: A)(implicit db: SQLiteDatabase): A = {
    insert(Seq(item))
    item
  }

  def delete(id: I)(implicit db: SQLiteDatabase) = db.delete(table.name, "_id = ?", Array(_id.encode(id)))

  def deleteAll(ids: TraversableOnce[I])(implicit db: SQLiteDatabase): Unit = inTransaction {
    withStatement(deleteSql) { stmt =>
      ids foreach { id =>
        stmt.bindString(1, _id.encode(id))
        stmt.execute()
      }
    }
  }

  def insertOrIgnore(items: TraversableOnce[A])(implicit db: SQLiteDatabase): Unit = insertWith(insertOrIgnoreSql)(items)

  def insert(items: TraversableOnce[A])(implicit db: SQLiteDatabase): Unit = insertWith(insertOrReplaceSql)(items)

  private def insertWith(sql: String)(items: TraversableOnce[A])(implicit db: SQLiteDatabase): Unit = inTransaction {
    withStatement(sql) { stmt =>
      items foreach { item =>
        stmt.bindString(1, _id.encode(getId(item)))
        stmt.bindString(2, encoder(item))
        // TODO: indexes
        stmt.execute()
      }
    }
  }
}

object Dao {

  def inTransaction[A](body: => A)(implicit db: SQLiteDatabase): A =
    if (db.inTransaction()) body
    else {
      db.beginTransaction()
      try {
        returning(body) { _ => db.setTransactionSuccessful() }
      } finally
        db.endTransaction()
    }

  def withStatement[A](sql: String)(body: SQLiteStatement => A)(implicit db: SQLiteDatabase): A = {
    val stmt = db.compileStatement(sql)
    try {
      body(stmt)
    } finally
      stmt.close()
  }
}
