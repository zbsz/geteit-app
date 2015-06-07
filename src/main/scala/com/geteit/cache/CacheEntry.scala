package com.geteit.cache

import java.io._

import com.geteit.app.GtContext
import com.geteit.inject.Injectable
import com.geteit.util.IoUtils
import com.geteit.util.Log._

class CacheEntry(val data: CacheEntryData)(implicit context: GtContext) extends Injectable {
  private implicit val logTag: LogTag = "CacheEntry"

  private lazy val service = inject[CacheService]

  def file = service.entryFile(data.fileId)

  def content = data.data

  def get: Either[File, Array[Byte]] = content.fold(Left(file): Either[File, Array[Byte]])(Right(_))

  def inputStream = content.fold[InputStream](new FileInputStream(file))(new ByteArrayInputStream(_))

  def outputStream = {
    file.getParentFile.mkdirs()
    new FileOutputStream(file)
  }

  def copyDataToFile() = {
    file.getParentFile.mkdirs()

    data.data foreach { data =>
      IoUtils.copy(new ByteArrayInputStream(data), new FileOutputStream(file))
    }
    file
  }

  def delete(): Unit = service.remove(this)
}

object CacheEntry {
  def unapply(entry: CacheEntry): Option[(String, Option[Array[Byte]], File)] = Some((entry.data.key, entry.content, entry.file))
}
