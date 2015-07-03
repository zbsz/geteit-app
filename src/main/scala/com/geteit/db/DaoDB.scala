package com.geteit.db

import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import com.geteit.util.Log._

trait DaoDB { self: SQLiteOpenHelper =>
  private implicit val tag: LogTag = "DaoDB"

  val daos: Seq[Dao[_, _]]
  val migrations: Seq[Migration]

  override def onCreate(db: SQLiteDatabase): Unit = {
    verbose(s"onCreate()")
    daos.foreach { dao =>
      verbose(s"creating table: ${dao.createTableSql}")
      db.execSQL(dao.createTableSql)
      dao.table.indexes foreach { index =>
        db.execSQL(index.createIndexSql(dao.table.name))
      }
    }
  }

  override def onUpgrade(db: SQLiteDatabase, from: Int, to: Int): Unit = {
    verbose(s"onUpgrade($from -> $to)")
    new Migrations(migrations: _*).migrate(this, from, to)(db)
  }

  def dropAllTables(db: SQLiteDatabase): Unit =
    daos.foreach { dao =>
      db.execSQL(dao.dropTableSql)
      dao.table.indexes foreach { index =>
        db.execSQL(index.dropIndexSql(dao.table.name))
      }
    }
}
