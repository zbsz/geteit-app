package com.geteit.db

import android.database.{DatabaseUtils, Cursor}
import android.database.sqlite.{SQLiteProgram, SQLiteDatabase, SQLiteStatement}
import com.geteit.db.DbType.Text
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
  def literal(v: A): String
  def bind(stmt: SQLiteProgram, pos: Int, v: A)
}
object DbType {
  trait Text { self: DbType[_] =>
    override val sqlName = "TEXT"
  }
  trait Integer { self: DbType[_] =>
    override val sqlName = "INTEGER"
  }

  implicit object StringDbType extends DbType[String] with Text {
    def literal(v: String) = s"'$v'"
    def bind(stmt: SQLiteProgram, pos: Int, v: String) = stmt.bindString(pos, v)
  }
  implicit object IntDbType extends DbType[Int] with Integer {
    def literal(v: Int) = v.toString
    def bind(stmt: SQLiteProgram, pos: Int, v: Int) = stmt.bindLong(pos, v)
  }
  implicit object LongDbType extends DbType[Long] with Integer {
    def literal(v: Long) = v.toString
    def bind(stmt: SQLiteProgram, pos: Int, v: Long) = stmt.bindLong(pos, v)
  }
  implicit object BoolDbType extends DbType[Boolean] with Integer {
    def literal(v: Boolean) = if (v) "1" else "0"
    def bind(stmt: SQLiteProgram, pos: Int, v: Boolean) = stmt.bindLong(pos, if (v) 1 else 0)
  }
}

case class Index[A, B](name: String, ext: A => B)(implicit val dbType: DbType[B]) {

  def createTableSql = s"$name ${dbType.sqlName}"
  def createIndexSql(table: String) = s"CREATE INDEX IF NOT EXISTS idx_${table}_$name on $table($name) "
  def dropIndexSql(table: String) = s"DROP INDEX IF EXISTS idx_${table}_$name"

  def bind(stmt: SQLiteProgram, pos: Int, v: A) = dbType.bind(stmt, pos, ext(v))

  def apply(item: A) = ext(item)
}

trait Matcher[A] {
  val whereSql: String
  def apply(item: A): Boolean
}

object Matcher {

  def all[A] = new Matcher[A] {
    override val whereSql: String = "1 = 1"
    override def apply(item: A): Boolean = true
  }

  def like[A](index: Index[A, String])(query: String): Matcher[A] = new Matcher[A] {
    override val whereSql: String = s"${index.name} LIKE '%$query%'"
    override def apply(item: A): Boolean = index(item).contains(query)
  }

  def startsWith[A](index: Index[A, String])(prefix: String): Matcher[A] = new Matcher[A] {
    override val whereSql: String = s"${index.name} LIKE '$prefix%'"
    override def apply(item: A): Boolean = index(item).startsWith(prefix)
  }

  def equal[A, B](index: Index[A, B])(v: B): Matcher[A] = new Matcher[A] {
    override val whereSql: String = s"${index.name} = ${index.dbType.literal(v)}"
    override def apply(item: A): Boolean = index(item) == v
  }

  def less[A, B: Ordering](index: Index[A, B])(v: B): Matcher[A] = new Matcher[A] {
    override val whereSql: String = s"${index.name} < ${index.dbType.literal(v)}"
    override def apply(item: A): Boolean = implicitly[Ordering[B]].compare(v, index(item)) < 0
  }

  def in[A, B](index: Index[A, B])(v: Set[B]): Matcher[A] = new Matcher[A] {
    override val whereSql: String = s"${index.name} in (${v.map(index.dbType.literal).mkString(",")})"
    override def apply(item: A): Boolean = v(index(item))
  }
}

abstract class Dao[I: Id, A: JsonDecoder : JsonEncoder] {
  import Dao._

  private val decoder = implicitly[JsonDecoder[A]]
  private val encoder = implicitly[JsonEncoder[A]]
  private val _id = implicitly[Id[I]]

  private val DataIndex = 1

  lazy val IdIndex = new Index[A, I]("_id", {_ => null.asInstanceOf[I] })(new DbType[I] with Text {
    override def literal(v: I): String = s"'${_id.encode(v)}'"
    override def bind(stmt: SQLiteProgram, pos: Int, v: I): Unit = stmt.bindString(pos, _id.encode(v))
  })

  val table: Table
  def getId(v: A): I

  case class Table(name: String, indexes: Seq[Index[A, _]])

  lazy val createTableSql = {
    val indexesSql = if (table.indexes.isEmpty) "" else table.indexes.map(_.createTableSql).mkString(", ", ", ", "")
    s"CREATE TABLE IF NOT EXISTS ${table.name} (_id TEXT PRIMARY KEY, _data TEXT $indexesSql)"
  }
  lazy val dropTableSql = s"DROP TABLE IF EXISTS ${table.name}"

  lazy val deleteSql = s"DELETE FROM ${table.name} WHERE _id = ?"

  private lazy val insertSql = {
    if (table.indexes.isEmpty) s"INTO ${table.name} (_id, _data) VALUES (?, ?)"
    else s"INTO ${table.name} (_id, _data ${table.indexes.map(_.name).mkString(", ", ", ", "")}) VALUES (${Seq.fill(table.indexes.size + 2)("?").mkString(", ")})"
  }
  lazy val insertOrReplaceSql = s"INSERT OR REPLACE $insertSql"
  lazy val insertOrIgnoreSql = s"INSERT OR IGNORE $insertSql"

  def decodeId(c: Cursor) = _id.decode(c.getString(0))

  def decode(c: Cursor) = decoder(c.getString(DataIndex))

  def single(c: Cursor) = try {
    if (c.moveToFirst()) Some(decode(c)) else None
  } finally c.close()

  def list(c: Cursor) = try {
    val builder = Seq.newBuilder[A]
    while (c.moveToNext()) builder += decode(c)
    builder.result()
  } finally c.close()

  def get(id: I)(implicit db: SQLiteDatabase): Option[A] =
    single(db.query(table.name, null, "_id = ?", Array(_id.encode(id)), null, null, null))

  def list(implicit db: SQLiteDatabase): Seq[A] = list(db.query(table.name, null, null, null, null, null, null))

  def query[K](ind: Index[A, K], value: K)(implicit db: SQLiteDatabase): Cursor =
    db.query(table.name, null, s"${ind.name} = ${ind.dbType.literal(value)}", null, null, null, null)

  def query(whereSql: String)(implicit db: SQLiteDatabase): Cursor =
    db.query(table.name, null, whereSql, null, null, null, null)

  def find(whereSql: String)(implicit db: SQLiteDatabase): Seq[I] = {
    val c = query(whereSql)
    try {
      val builder = Seq.newBuilder[I]
      while (c.moveToNext()) builder += _id.decode(c.getString(0))
      builder.result()
    } finally c.close()
  }

  def count(whereSql: String)(implicit db: SQLiteDatabase): Long =
    db.compileStatement(s"select count(*) from ${table.name} where $whereSql;").simpleQueryForLong()

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
        table.indexes.zipWithIndex foreach { case (index, i) =>
          index.bind(stmt, i + 3, item)
        }
        stmt.execute()
      }
    }
  }
}

object Dao {

  def inTransaction[A](body: => A)(implicit db: SQLiteDatabase): A = Storage.inTransaction(body)

  def withStatement[A](sql: String)(body: SQLiteStatement => A)(implicit db: SQLiteDatabase): A = {
    val stmt = db.compileStatement(sql)
    try {
      body(stmt)
    } finally
      stmt.close()
  }
}
