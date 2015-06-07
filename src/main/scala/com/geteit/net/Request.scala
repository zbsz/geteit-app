package com.geteit.net

import java.io.{File, InputStream}
import java.net.URLEncoder

import android.net.Uri
import android.net.http.AndroidHttpClient
import com.geteit.app.GtContext
import com.geteit.net.ContentEncoder.{EmptyContentEncoder, EmptyRequestContent, RequestContent}
import com.geteit.net.Request.ProgressCallback
import com.geteit.util.{IoUtils, _}
import com.koushikdutta.async.callback.CompletedCallback
import com.koushikdutta.async.http.AsyncHttpRequest
import com.koushikdutta.async.http.body.{AsyncHttpRequestBody, MultipartFormDataBody}
import com.koushikdutta.async.{DataEmitter, DataSink, Util}
import org.json.JSONObject

import scala.concurrent.duration._

case class Request[A: ContentEncoder](
                                       uri: Uri,
                                       httpMethod: String = Request.GetMethod,
                                       data: Option[A] = None,
                                       decoder: Option[ResponseBodyDecoder] = None,
                                       callback: Option[ProgressCallback] = None,
                                       headers: Map[String, String] = Request.EmptyHeaders,
                                       timeout: FiniteDuration = AsyncClient.DefaultTimout
                                     ) {


  def getBody = data.map(implicitly[ContentEncoder[A]].apply).getOrElse(EmptyRequestContent)
}

object Request {
  type ProgressCallback = Progress => Unit

  val PostMethod = "POST"
  val PutMethod = "PUT"
  val GetMethod = "GET"
  val DeleteMethod = "DELETE"
  val HeadMethod = "HEAD"

  val EmptyHeaders = Map[String, String]()

  def Post[A: ContentEncoder](uri: Uri, data: A, headers: Map[String, String] = EmptyHeaders, timeout: FiniteDuration = AsyncClient.DefaultTimout) =
    Request[A](uri, PostMethod, data = Some(data), headers = headers, timeout = timeout)

  def Put[A: ContentEncoder](uri: Uri, data: A, headers: Map[String, String] = EmptyHeaders) =
    Request[A](uri, PutMethod, data = Some(data), headers = headers)

  def Delete(uri: Uri, headers: Map[String, String] = EmptyHeaders) =
    Request[Unit](uri, DeleteMethod, headers = headers)(EmptyContentEncoder)

  def Get(uri: Uri, callback: Option[ProgressCallback] = None, headers: Map[String, String] = EmptyHeaders) =
    Request[Unit](uri, GetMethod, callback = callback, headers = headers)(EmptyContentEncoder)

  def RangeGet(uri: Uri, range: RangeSpec, dst: File, callback: Option[ProgressCallback] = None, headers: Map[String, String] = EmptyHeaders)(implicit context: GtContext) = {
    Request[Unit](uri, GetMethod, callback = callback, headers = headers + RangeSpec(range), decoder = Some(new RangeResponseBodyDecoder(dst)))(EmptyContentEncoder)
  }

  def query(path: String, args: (String, Any)*): String = {
    args map {
      case (key, value) =>
        URLEncoder.encode(key, "utf8") + "=" + URLEncoder.encode(value.toString, "utf8")
    } mkString (path + (if (path.contains('?')) "&" else "?"), "&", "")
  }
}

trait ContentEncoder[A] { self =>
  def apply(data: A): RequestContent

  def map[B](f: B => A): ContentEncoder[B] = new ContentEncoder[B] {
    override def apply(data: B): RequestContent = self(f(data))
  }
}

object ContentEncoder {

  sealed trait RequestContent {
    protected def asyncHttpBody: Option[AsyncHttpRequestBody[_]] = None

    def apply(req: AsyncHttpRequest): AsyncHttpRequest = {
      asyncHttpBody foreach req.setBody
      req
    }
  }

  case object EmptyRequestContent extends RequestContent

  trait ByteArrayRequestContent extends RequestContent {
    def data: Array[Byte]
    def contentType: String

    override def asyncHttpBody = Some(new AsyncHttpRequestBody[Unit] {
      override def get(): Unit = ()
      override def length(): Int = data.length
      override def readFullyOnRequest(): Boolean = false
      override def getContentType: String = contentType
      override def parse(emitter: DataEmitter, completed: CompletedCallback): Unit = { throw new UnsupportedOperationException("Temp request body should only be used for writing") }
      override def write(request: AsyncHttpRequest, sink: DataSink, completed: CompletedCallback): Unit = Util.writeAll(sink, data, completed)
    })
  }

  case class BinaryRequestContent(data: Array[Byte], contentType: String) extends ByteArrayRequestContent {
    override def toString: String = {
      val ct = Option(contentType)
      if (ct.exists(_.contains("text")) || ct.exists(_.contains("json")))
        s"BinaryRequestContent(${new String(data, "utf8")}, $ct)"
      else
        s"BinaryRequestContent(data len:${data.length}, $ct)"
    }

    def gzipped = GzippedRequestContent(data, contentType)
  }

  case class GzippedRequestContent(bytes: Array[Byte], contentType: String) extends ByteArrayRequestContent {

    override lazy val data = {
      if (bytes.length <= AndroidHttpClient.DEFAULT_SYNC_MIN_GZIP_BYTES) bytes
      else {
        val zip = IoUtils.gzip(bytes)
        if (zip.length >= bytes.length) bytes
        else zip
      }
    }

    override def apply(req: AsyncHttpRequest): AsyncHttpRequest = {
      if (data ne bytes) req.setHeader("Content-Encoding", "gzip")
      super.apply(req)
    }

    override def toString: String = {
      val ct = Option(contentType)
      if (ct.exists(_.contains("text")) || ct.exists(_.contains("json")))
        s"GzippedRequestContent(${new String(data, "utf8")}, $ct)"
      else
        s"GzippedRequestContent(data len:${data.length}, $ct)"
    }
  }

  case class StreamRequestContent(stream: InputStream, contentType: String, length: Int) extends RequestContent { self =>
    override def asyncHttpBody = Some(new AsyncHttpRequestBody[Unit] {
      override def get(): Unit = ()
      override def length(): Int = self.length
      override def readFullyOnRequest(): Boolean = false
      override def getContentType: String = self.contentType
      override def parse(emitter: DataEmitter, completed: CompletedCallback): Unit = { throw new UnsupportedOperationException("Temp request body should only be used for writing") }
      override def write(request: AsyncHttpRequest, sink: DataSink, completed: CompletedCallback): Unit = Util.pump(stream, if (self.length < 0) Integer.MAX_VALUE else self.length, sink, completed)
    })
  }

  case class MultipartRequestContent(files: Seq[(String, File)]) extends RequestContent {
    override def asyncHttpBody = Some(returning(new MultipartFormDataBody()) { mp =>
      files.foreach {
        case (name, file) => mp.addFilePart(name, file)
      }
    })
  }

  implicit object RequestContentEncoder extends ContentEncoder[RequestContent] {
    override def apply(data: RequestContent) = data
  }

  implicit object BinaryContentEncoder extends ContentEncoder[BinaryRequestContent] {
    override def apply(data: BinaryRequestContent) = data
  }

  implicit object GzippedContentEncoder extends ContentEncoder[GzippedRequestContent] {
    override def apply(data: GzippedRequestContent) = data
  }

  implicit object EmptyContentEncoder extends ContentEncoder[Unit] {
    override def apply(data: Unit) = EmptyRequestContent
  }

  implicit object StringContentEncoder extends ContentEncoder[String] {
    override def apply(data: String) = new BinaryRequestContent(data.getBytes("utf8"), "text/plain")
  }

  implicit object JsonContentEncoder extends ContentEncoder[JSONObject] {
    override def apply(data: JSONObject) = new BinaryRequestContent(data.toString.getBytes("utf8"), "application/json")
  }

  implicit object FormDataContentEncoder extends ContentEncoder[Map[String, String]] {
    override def apply(data: Map[String, String]) = {
      val content = data.map { case (k, v) => URLEncoder.encode(k, "utf8") + "=" + URLEncoder.encode(v, "utf8") } .mkString("&")
      new BinaryRequestContent(content.toString.getBytes("utf8"), "application/x-www-form-urlencoded")
    }
  }
}
