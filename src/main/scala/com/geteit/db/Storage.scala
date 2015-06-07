package com.geteit.db

import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import com.geteit.concurrent.LimitedExecutionContext
import com.geteit.inject.Factory
import com.geteit.util.Log._
import com.geteit.util.returning

import scala.concurrent.Future
import scala.util.control.NonFatal

trait Storage {
  import Storage._
  implicit val ec = new LimitedExecutionContext()

  val dbHelper: SQLiteOpenHelper

  def apply[A](f: SQLiteDatabase => A, autoCommit: Boolean = true): Future[A] = Future {
    f(dbHelper.getWritableDatabase)
  }

  def withTransaction[A](f: SQLiteDatabase => A, autoCommit: Boolean = true): Future[A] = Future {
    val db = dbHelper.getWritableDatabase
    db.beginTransaction()
    try returning(f(db)) { _ => if (autoCommit) db.setTransactionSuccessful() }
    catch { case NonFatal(e) => error(s"withTransaction failed with error", e); throw e }
    finally db.endTransaction()
  }

  def close() = Future {
    dbHelper.close()
  }
}

object Storage {
  private implicit val tag: LogTag = "Storage"

  implicit val factory = new Factory[Storage](_ => throw new UnsupportedOperationException("Injectable storage should be provided"))
}
