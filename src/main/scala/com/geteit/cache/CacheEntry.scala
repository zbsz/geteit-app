package com.geteit.cache

import java.io._

import com.geteit.util.IoUtils
import com.geteit.util.Log._
import com.geteit.inject.{Injectable, Injector}

class CacheEntry(val data: CacheEntryData)(implicit inj: Injector) extends Injectable {
  private implicit val logTag: LogTag = "CacheEntry"

  private lazy val service = inject[CacheService]

  def file = service.entryFile(data.fileId)

  def content = data.data

  def get: Either[File, Array[Byte]] = content.fold(Left(file): Either[File, Array[Byte]])(Right(_))

  def getData: Array[Byte] = content.getOrElse(IoUtils.toByteArray(new FileInputStream(file)))

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
