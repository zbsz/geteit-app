package com.geteit.net

import java.io.File

import android.content.Context
import com.geteit.app.GtContext
import com.geteit.cache.CacheService
import com.geteit.net.ResponseConsumer._
import com.geteit.util.Log._
import com.koushikdutta.async.http.{Headers => KoushHeaders}
import com.geteit.inject.{Injectable, Injector}

case class Response(
                     status: Response.Status,
                     body: ResponseContent = EmptyResponse,
                     headers: Response.Headers = Response.EmptyHeaders
                     )

object Response {

  object Status {
    val Success = 200
    val Unauthorized = 401
    val Forbidden = 403
    val RateLimiting = 420
    val LoginRateLimiting = 429
    val Conflict = 409

    def isFatal(status: Int) = status != Unauthorized && status != RateLimiting && status / 100 == 4
  }

  object ErrorStatus {
    def unapply(status: Status) = !status.isSuccess
  }
  object ServerErrorStatus {
    def unapply(status: Status) = status match {
      case HttpStatus(s, _) => s / 100 == 5
      case _ => false
    }
  }
  object SuccessStatus {
    def unapply(status: Status) = status.isSuccess
  }
  object SuccessHttpStatus {
    def unapply(status: Status) = status match {
      case s: HttpStatus => s.isSuccess
      case _ => false
    }
  }


  sealed trait Status {
    def isSuccess: Boolean
    def msg: String
    val status: Int

    def isFatal = Status.isFatal(status)
  }

  case class HttpStatus(status: Int, msg: String = "") extends Status {
    override def isSuccess: Boolean = status / 100 == 2
  }

  /**
   * Response status indicating some internal exception happening during request or response processing.
   * This should generally indicate a bug in our code and we don't know if the request was processed by server.
   */
  case class InternalError(msg: String, cause: Option[Throwable] = None, httpStatus: Option[HttpStatus] = None) extends Status {
    override val isSuccess: Boolean = false
    override val status: Int = 600
  }

  /**
   * Response status indicating internet connection problem.
   */
  case class ConnectionError(msg: String) extends Status {
    override val isSuccess: Boolean = false
    override val status: Int = 601
  }

  case object Cancelled extends Status {
    override val msg = "Cancelled by user"
    override val isSuccess = false
    override val status: Int = 602
  }

  case object ClientClosed extends Status {
    override val msg = "Client has been closed"
    override val isSuccess = false
    override val status: Int = 603
  }

  val EmptyHeaders = new Headers

  class Headers(val headers: KoushHeaders = new KoushHeaders()) {
    import scala.collection.JavaConverters._
    lazy val map = headers.getMultiMap

    def apply(key: String): Option[String] = Option(headers.get(key))

    def foreach(key: String)(f: String => Unit): Unit = Option(headers.getAll(key)).foreach(_.iterator().asScala foreach f)

    override def toString: String = s"Headers[${map.entrySet().asScala map { e => e.getKey -> e.getValue.asScala }}]"
  }

  object Headers {
    def apply(entries: (String, String)*) = {
      val headers = new KoushHeaders()
      entries foreach { case (key, value) => headers.add(key, value) }
      new Headers(headers)
    }
  }
}

trait ResponseBodyDecoder {
  def apply(headers: KoushHeaders, contentLength: Long): ResponseConsumer[_ <: ResponseContent]
}

class DefaultResponseBodyDecoder(implicit inj: Injector) extends ResponseBodyDecoder with Injectable {
  val TextContent = "text/.*".r
  val JsonContent = "application/json.*".r
  val ImageContent = "image/.*".r
  val InMemoryThreshold = 24 * 1024

  lazy val cache = inject[CacheService]

  def apply(headers: KoushHeaders, contentLength: Long): ResponseConsumer[_ <: ResponseContent] = {
    val contentType = Option(headers.get("Content-Type")).getOrElse("")

    contentType match {
      case JsonContent() => new StringConsumer(contentLength) // TODO: can we support streaming json? we don't want to fallback to generic DOM ?
      case TextContent() => new StringConsumer(contentLength)
      case _ if contentLength > InMemoryThreshold => new FileConsumer(contentType)(cache)
      case _ => new ByteArrayConsumer(contentLength, contentType)
    }
  }
}

class RangeResponseBodyDecoder(dst: File)(implicit inj: Injector) extends DefaultResponseBodyDecoder {
  private implicit val tag: LogTag = "RangeResponseBodyDecoder"

  override def apply(headers: KoushHeaders, contentLength: Long): ResponseConsumer[_ <: ResponseContent] = {
    val contentType = Option(headers.get("Content-Type")).getOrElse("")
    headers.get(ContentRange.Header) match {
      case ContentRange(range) => new RangeFileConsumer(dst, range, contentType)
      case header =>
        warn(s"Unexpected Content-Range header: $header")
        super.apply(headers, contentLength)
    }
  }
}
