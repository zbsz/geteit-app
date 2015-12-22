package com.geteit.db

import android.annotation.TargetApi
import android.database.sqlite._
import android.os.{CancellationSignal, Build}
import com.geteit.concurrent.{Threading, LimitedExecutionContext}
import com.geteit.util.Log._
import com.geteit.util.returning

import scala.concurrent.Future
import scala.util.Try

trait Storage {
  import Storage._
  implicit val ec = new LimitedExecutionContext(parent = Threading.io)

  val dbHelper: SQLiteOpenHelper

  private def getWritable = returning(dbHelper.getWritableDatabase)(transactions.enableWal)
  private def getReadable = returning(dbHelper.getReadableDatabase)(transactions.enableWal)

  def apply[A](f: SQLiteDatabase => A): Future[A] = Future {
    implicit val db = getWritable
    inTransaction(f(db))
  }

  def read[A](f: SQLiteDatabase => A): Future[A] = Future {
    implicit val db = getReadable
    inReadTransaction(f(db))
  } (Threading.io)

  def close() = Future {
    dbHelper.close()
  }
}

object Storage {
  private implicit val tag: LogTag = "Storage"

  private lazy val transactions = TransactionSupport()

  def inReadTransaction[A](body: => A)(implicit db: SQLiteDatabase): A = {
    transactions.beginReadTransaction(db)
    try returning(body) { _ => db.setTransactionSuccessful() }
    finally db.endTransaction()
  }

  def inTransaction[A](body: => A)(implicit db: SQLiteDatabase): A = {
    transactions.beginTransaction(db)
    try returning(body) { _ => db.setTransactionSuccessful() }
    finally db.endTransaction()
  }

  trait TransactionSupport {
    def beginTransaction(db: SQLiteDatabase): Unit = db.beginTransaction()
    def beginReadTransaction(db: SQLiteDatabase): Unit = beginTransaction(db)
    def enableWal(db: SQLiteDatabase): Unit = ()
  }

  object TransactionSupport {
    private implicit val logTag: LogTag = logTagFor[TransactionSupport]
    def apply(): TransactionSupport =
      if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB) new TransactionSupport {}
      else if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN) NonExclusiveTransactionSupport
      else Try(DeferredModeTransactionSupport()).getOrElse(NonExclusiveTransactionSupport)
  }

  @TargetApi(16)
  object DeferredModeTransactionSupport {
    def apply(): TransactionSupport = new TransactionSupport {
      private val getThreadSession = classOf[SQLiteDatabase].getDeclaredMethod("getThreadSession")
      private val sessionCls = Class.forName("android.database.sqlite.SQLiteSession")
      private val beginTrans = sessionCls.getDeclaredMethod("beginTransaction", classOf[Int], classOf[SQLiteTransactionListener], classOf[Int], classOf[CancellationSignal])
      getThreadSession.setAccessible(true)

      override def beginTransaction(db: SQLiteDatabase): Unit = db.beginTransactionNonExclusive()
      override def beginReadTransaction(db: SQLiteDatabase): Unit = try reflectiveBegin(db) catch { case _: Exception => db.beginTransactionNonExclusive() }
      override def enableWal(db: SQLiteDatabase): Unit = db.enableWriteAheadLogging()

      private def reflectiveBegin(db: SQLiteDatabase): Unit = {
        db.acquireReference()
        try {
//        db.getThreadSession.beginTransaction(SQLiteSession.TRANSACTION_MODE_DEFERRED, null, SQLiteConnectionPool.CONNECTION_FLAG_READ_ONLY, null)
          beginTrans.invoke(getThreadSession.invoke(db), Integer.valueOf(0), null, Integer.valueOf(1), null)
        } catch {
          case e: Exception => error("reflectiveBegin failed", e)
        }
        finally db.releaseReference()
      }
    }
  }

  @TargetApi(11)
  object NonExclusiveTransactionSupport extends TransactionSupport {
    override def beginTransaction(db: SQLiteDatabase): Unit = db.beginTransactionNonExclusive()
    override def enableWal(db: SQLiteDatabase): Unit = db.enableWriteAheadLogging()
  }
}
