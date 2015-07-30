package com.geteit.net

import java.io._

import com.geteit.cache.CacheService
import com.geteit.util.Log._
import com.geteit.util._
import com.koushikdutta.async.ByteBufferList

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

sealed trait ResponseContent
sealed trait JsonResponse extends ResponseContent
case object EmptyResponse extends ResponseContent
case class StringResponse(value: String) extends ResponseContent
case class BinaryResponse(value: Array[Byte], mime: String) extends ResponseContent {
  override def toString: String = s"BinaryResponse(${new String(value.take(1024))}, $mime)"
}
case class FileResponse(value: File, mime: String) extends ResponseContent

trait ResponseConsumer[T <: ResponseContent] {
  def consume(bb: ByteBufferList): Unit
  def result: Try[T]
}

object ResponseConsumer {

  private def copyToStream(bb: ByteBufferList, out: OutputStream): Unit = {
    while (bb.size() > 0) {
      val b = bb.remove()
      out.write(b.array(), b.arrayOffset() + b.position(), b.remaining())
      ByteBufferList.reclaim(b)
    }
  }

  object EmptyResponseConsumer extends ResponseConsumer[ResponseContent] {
    override def result = Success(EmptyResponse)
    override def consume(bb: ByteBufferList): Unit = bb.recycle()
  }

  trait InMemoryConsumer[T <: ResponseContent] extends ResponseConsumer[T] {
    val length: Long
    val data = if (length > 0) new ByteArrayOutputStream(length.toInt) else new ByteArrayOutputStream()

    override def consume(bb: ByteBufferList): Unit = copyToStream(bb, data)
  }

  class ByteArrayConsumer(val length: Long, mime: String) extends InMemoryConsumer[BinaryResponse] {
    override def result = Success(BinaryResponse(data.toByteArray, mime))
  }

  class StringConsumer(val length: Long) extends InMemoryConsumer[StringResponse] {
    override def result = Success(StringResponse(data.toString("utf8")))
  }

  class FileConsumer(mime: String)(cache: CacheService) extends ResponseConsumer[FileResponse] {
    val entry = cache.createForFile()(10.minutes)
    val out = entry.outputStream
    var ex = None: Option[Throwable]

    override def consume(bb: ByteBufferList): Unit = {
      try {
        copyToStream(bb, out)
      } catch {
        case e: Throwable =>
          bb.recycle()
          ex = ex.orElse(Some(e))
      }
    }

    override def result: Try[FileResponse] = {
      try {
        out.close()
        ex.fold(Success(FileResponse(entry.file, mime)): Try[FileResponse]) { e => Failure(e) }
      } catch {
        case e: Throwable =>
          Failure(ex.getOrElse(e))
      }
    }
  }

  class RangeFileConsumer(file: File, range: ContentRange, mime: String) extends ResponseConsumer[FileResponse] {
    private implicit val tag: LogTag = "RangeFileConsumer"

    lazy val raf = returning(new RandomAccessFile(file, "rw")) { f =>
      f.seek(range.from)
    }
    var ex = None: Option[Throwable]

    override def consume(bb: ByteBufferList): Unit = {
      try {
        while (bb.size() > 0) {
          val b = bb.remove()
          raf.write(b.array(), b.arrayOffset() + b.position(), b.remaining())
          ByteBufferList.reclaim(b)
        }
      } catch {
        case e: Throwable =>
          bb.recycle()
          ex = ex.orElse(Some(e))
      }
    }

    override def result: Try[FileResponse] = {
      try {
        raf.close()
        ex.fold(Success(FileResponse(file, mime)): Try[FileResponse]) { e => Failure(e) }
      } catch {
        case e: Throwable =>
          Failure(ex.getOrElse(e))
      }
    }
  }
}
