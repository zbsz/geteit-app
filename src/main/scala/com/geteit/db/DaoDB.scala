package com.geteit.db

import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}

trait DaoDB { self: SQLiteOpenHelper =>

  val daos: Seq[Dao[_, _]]
  val migrations: Seq[Migration]

  override def onCreate(db: SQLiteDatabase): Unit =
    daos.foreach { dao =>
      db.execSQL(dao.createTableSql)
    }

  override def onUpgrade(db: SQLiteDatabase, from: Int, to: Int): Unit =
    new Migrations(migrations: _*).migrate(this, from, to)(db)

  def dropAllTables(db: SQLiteDatabase): Unit =
    daos.foreach { dao => db.execSQL(dao.dropTableSql) }
}
