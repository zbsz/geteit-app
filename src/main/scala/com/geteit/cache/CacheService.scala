package com.geteit.cache

import java.io._

import com.geteit.util.IoUtils
import com.geteit.util.Log._
import com.geteit.inject.{Injectable, Injector}

import scala.concurrent.Future
import scala.concurrent.duration._


case class Expiration(timeout: Long)

object Expiration {
  import scala.language.implicitConversions

  implicit def in(d: Duration) : Expiration = if (d.isFinite()) Expiration(d.toMillis) else Expiration(100L * 3600L * 24L * 365L * 1000L) // 100 years (don't use Long.MaxValue due to overflow dangers)
}

class CacheService(implicit inj: Injector) extends Injectable {
  private implicit val logTag: LogTag = "CacheService"
  import com.geteit.concurrent.Threading.global

  val storage = inject[CacheStorage]

  def createForFile(key: String = Uid().str)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CacheEntry = add(CacheEntryData(key))

  def addData(key: String, data: Array[Byte])(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CacheEntry = {
    add(key, Left(data))(timeout)
  }

  def addStream(key: String, in: => InputStream)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): Future[CacheEntry] = Future {
    val data = CacheEntryData(key, timeout = timeout.timeout)
    val file = entryFile(data.fileId)
    try {
      file.getParentFile.mkdirs()
      IoUtils.copy(in, new FileOutputStream(file))
      add(data)
    } catch {
      case e: IOException =>
        error(s"addStream($key) failed, will return expired cache entry", e)
        add(data.copy(lastUsed = 0L)) // already expired
    }
  }

  def addFile(key: String, src: File, moveFile: Boolean = false)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): Future[CacheEntry] = Future {
    val data = CacheEntryData(key, timeout = timeout.timeout)
    val file = entryFile(data.fileId)
    try {
      file.getParentFile.mkdirs()
      IoUtils.copy(new FileInputStream(src), new FileOutputStream(file))
      if (moveFile) src.delete()
      add(data)
    } catch {
      case e: IOException =>
        error(s"addFile($key) failed, will return expired cache entry", e)
        add(data.copy(lastUsed = 0L)) // already expired
    }
  }

  def move(key: String, entry: CacheEntry)(implicit timeout: Expiration = CacheService.DefaultExpiryTime) = {
    (entry.get match {
      case Left(file) => addFile(key, file, moveFile = true)
      case Right(data) => Future.successful(addData(key, data))
    }) map { current =>
      entry.delete()
      current
    }
  }

  // You can either add byte data directly (for previews or otherwise very small entries), or you can add files.
  // When adding files, you can optionally specify a parent location under which to put them.
  private def add(key: String, data: Either[Array[Byte], Option[File]])(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CacheEntry = {
    add(CacheEntryData(key, data.left.toOption, timeout = timeout.timeout))
  }

  private def add(entry: CacheEntryData) = {
    storage.add(entry)
    new CacheEntry(entry)
  }

  def getEntry(key: String): Future[Option[CacheEntry]] = storage.get(key) map {
    case Some(e) => Some(new CacheEntry(e))
    case None => None
  }

  def getOrElse(key: String, default: => Future[CacheEntry]) = getEntry(key) flatMap {
    case Some(entry) => Future.successful(entry)
    case _ => default
  }

  def remove(key: String): Unit = storage.remove(key)

  def remove(entry: CacheEntry): Unit = storage.remove(entry.data)

  private[cache] def entryFile(fileId: Uid) = storage.entryFile(fileId)
}

object CacheService {
  val DefaultExpiryTime = 7.days
}
