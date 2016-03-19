package com.geteit.cache

import java.io.File
import java.lang.System._

import android.content.Context
import com.geteit.cache.CacheEntryData.CacheEntryDao
import com.geteit.concurrent.Threading
import com.geteit.db.CachedStorage
import com.geteit.events.EventContext
import com.geteit.inject.{Injectable, Injector}
import com.geteit.util.Log._
import com.geteit.util._

import scala.concurrent.Future

class CacheStorage(implicit inj: Injector) extends CachedStorage[String, CacheEntryData] with Injectable {
  private implicit val logTag: LogTag = "CacheStorage"
  private implicit val eventContext = inject[EventContext]

  lazy val cacheDir = CacheStorage.cacheDir(inject[Context])
  
  override protected val cache = new LruCache[String, Option[CacheEntryData]](512 *1024) {
    override def sizeOf(key: String, value: Option[CacheEntryData]): Int =
      value.flatMap(_.data).fold(0)(_.length) + key.length + 128 // data plus some object overhead
  }

  val fileCleanupQueue = new SerialProcessingQueue[Uid]({ entries =>
    Future { entries foreach { entryFile(_).delete() } }(Threading.global)
  }, "CacheFileCleanupQueue")

  private def filterValid(entry: Option[CacheEntryData]) = entry match {
    case Some(e) if expired(e) || dataMissing(e) =>
      remove(e)
      None
    case res => res
  }

  override def get(key: String): Future[Option[CacheEntryData]] = super.get(key) map filterValid

  override def getAll(keys: Traversable[String]): Future[Seq[Option[CacheEntryData]]] = super.getAll(keys) map { _ map filterValid }

  def entryFile(id: Uid) = CacheStorage.entryFile(cacheDir, id)
  
  def updateExpires(key: String) = update(key, _.copy(lastUsed = System.currentTimeMillis()))

  def remove(entry: CacheEntryData): Unit = {
    fileCleanupQueue ! entry.fileId
    super.remove(entry.key)
  }

  private def expired(entry: CacheEntryData) = entry.lastUsed + entry.timeout <= System.currentTimeMillis()

  private def dataMissing(entry: CacheEntryData) = entry.data.isEmpty && !entryFile(entry.fileId).exists()

  def deleteExpired(): Future[Unit] = {
    val currentTime = currentTimeMillis()
    storage { implicit db =>
      val entries = CacheEntryDao.findAllExpired(currentTime)
      CacheEntryDao.deleteExpired(currentTime)
      entries
    }.map { entries =>
      entries foreach remove
    }.flatMap { _ =>
      fileCleanupQueue.post(Future.successful(()))
    }
  }
}

object CacheStorage {
  def cacheDir(context: Context) = returning(new File(Option(context.getExternalCacheDir).getOrElse(context.getCacheDir), "cache_entries")) { dir => dir.mkdirs() }
  
  def entryFile(cacheDir: File, uid: Uid) = new File(new File(cacheDir, uid.str.take(2)), uid.str)
}
