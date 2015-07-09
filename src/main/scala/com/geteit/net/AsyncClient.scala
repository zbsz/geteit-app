package com.geteit.net

import java.net.ConnectException
import java.util.concurrent.atomic.AtomicLong

import android.content.Context
import android.net.Uri
import com.geteit.app.GtContext
import com.geteit.concurrent.CancellableFuture.CancelException
import com.geteit.concurrent.{CancellableFuture, LimitedExecutionContext}
import com.geteit.net.Request.ProgressCallback
import com.geteit.net.Response.HttpStatus
import com.geteit.util.Log._
import com.koushikdutta.async._
import com.koushikdutta.async.callback.DataCallback.NullDataCallback
import com.koushikdutta.async.callback.{CompletedCallback, DataCallback}
import com.koushikdutta.async.http._
import com.koushikdutta.async.http.callback.HttpConnectCallback
import com.geteit.inject.{Injectable, Injector}

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class AsyncClient(implicit inj: Injector) extends Injectable {
  import AsyncClient._

  implicit val dispatcher = new LimitedExecutionContext()

  lazy val cookies = inject[CookieStorage]
  lazy val bodyDecoder = inject[ResponseBodyDecoder]

  lazy val userAgent = AsyncClient.userAgent(inject[Context])

  val client = ClientWrapper { new AsyncHttpClient(new AsyncServer) }

  def apply[A](request: Request[A]): CancellableFuture[Response] = {
    debug(s"Starting request: $request")

    client .flatMap { client =>
      CancellableFuture.lift(buildHttpRequest(request)) .flatMap { httpRequest =>
        val p = Promise[Response]()
        @volatile var cancelled = false
        @volatile var processFuture = None: Option[CancellableFuture[_]]

        val httpFuture = client.execute(httpRequest, new HttpConnectCallback {
          override def onConnectCompleted(ex: Exception, response: AsyncHttpResponse): Unit = {
            debug(s"Connect completed for uri: '${request.uri}', ex: '$ex', cancelled: $cancelled")

            if (ex != null) p.tryFailure(ex)
            else {
              val future = processResponse(request.uri, response, request.decoder.getOrElse(bodyDecoder), request.callback)
              future.onComplete(p.tryComplete)

              // XXX: order is important here, we first set processFuture and then check cancelled to avoid race condition in cancel callback
              processFuture = Some(future)
              if (cancelled) future.cancel()
            }
          }
        })

        new CancellableFuture(p) {
          override def cancel(): Boolean = {
            debug(s"cancelling request: $request")
            cancelled = true
            httpFuture.cancel(true)
            processFuture.foreach(_.cancel())
            super.cancel()
          }
        }.recover(exceptionStatus).withTimeout(request.timeout) // this is a quick fix for AndroidAsync sometimes not properly finishing handling failed requests due to closed keep-alive sockets
      }
    }
  }

  def close(): Unit = client map { _.getServer.stop() }

  private def buildHttpRequest(req: Request[_]): Future[AsyncHttpRequest] = cookies.header(req.uri) map { cookie =>
    val r = new AsyncHttpRequest(req.uri, req.httpMethod)
    r.setTimeout(req.timeout.toMillis.toInt)
    req.headers.foreach(p => r.addHeader(p._1, p._2))
    r.setHeader("User-Agent", userAgent)
    cookie foreach { case (k, v) => r.setHeader(k, v) }
    req.getBody(r)
  }

  //XXX: has to be executed on Http thread (inside onConnectCompleted), since data callbacks have to be set before this callback completes,
  private def processResponse(uri: Uri, response: AsyncHttpResponse, decoder: ResponseBodyDecoder = bodyDecoder, progressCallback: Option[ProgressCallback]): CancellableFuture[Response] = {
    val httpStatus = HttpStatus(response.code(), response.message())
    val contentLength = HttpUtil.contentLength(response.headers())

    cookies.update(uri, response.headers())

    val range = Option(response.headers().get(ContentRange.Header)) match {
      case Some(ContentRange(rng)) => rng
      case _ => ContentRange(0, contentLength, contentLength)
    }

    debug(s"got connection response for request: $uri, status: '$httpStatus', length: '$contentLength', headers: '${response.headers()}'")
    verbose(s"decoder: $decoder")

    progressCallback foreach (_(Progress(0L, range, Progress.Running)))
    if (contentLength == 0) CancellableFuture.successful(Response(httpStatus))
    else {
      debug(s"waiting for content from $uri")

      val p = Promise[Response]()
      val consumer = decoder(response.headers(), contentLength)

      response.setDataCallback(new DataCallback {
        var bytesReceived = new AtomicLong(0L)

        override def onDataAvailable(emitter: DataEmitter, bb: ByteBufferList): Unit = {
          debug(s"data received for $uri, length: ${bb.remaining}")
          val numConsumed = bb.remaining
          consumer.consume(bb)
          progressCallback foreach { cb => Future(cb(Progress(bytesReceived.addAndGet(numConsumed), range, Progress.Running))) }
        }
      })

      response.setEndCallback(new CompletedCallback {
        override def onCompleted(ex: Exception): Unit = {
          debug(s"response for $uri ENDED, ex: $ex, p.isCompleted: ${p.isCompleted}")
          if (ex != null) ex.printStackTrace(Console.err)
          response.setDataCallback(null)
          p.tryComplete(
            if (ex != null) Failure(ex)
            else consumer.result match {
              case Success(body) =>
                progressCallback foreach { cb => Future(cb(Progress(contentLength, range, Progress.Done))) }
                Success(Response(httpStatus, body, new Response.Headers(response.headers())))

              case Failure(t) =>
                progressCallback foreach { cb => Future(cb(Progress(0, range, Progress.Failed(t)))) }
                Success(Response(Response.InternalError(s"Response body consumer failed for request: $uri", Some(t), Some(httpStatus))))
            }
          )
        }
      })

      new CancellableFuture(p) {
        override def cancel(): Boolean = {
          debug(s"cancelling response processing for: $uri")
          response.setDataCallback(new NullDataCallback)
          response.setEndCallback(null)
          response.close()
          progressCallback foreach { cb => Future(cb(Progress(0, range, Progress.Cancelled))) }
          super.cancel()
        }
      }
    }
  }
}

object AsyncClient {
  private implicit val logTag: LogTag = "AsyncClient"
  val DefaultTimout = 5.minutes
  val EmptyHeaders = Map[String, String]()

  def userAgent(implicit context: Context) = {
    import android.os.Build._
    val appVersion = context.getPackageManager.getPackageInfo(context.getPackageName, 0).versionName
    s"${context.getPackageName}/$appVersion (Android ${VERSION.RELEASE}; $MANUFACTURER $MODEL)"
  }

  private def exceptionStatus: PartialFunction[Throwable, Response] = {
    case CancelException => throw CancelException
    case e: ConnectException => Response(Response.ConnectionError(e.getMessage))
    // TODO: handle connection exceptions
    case NonFatal(e) => Response(Response.InternalError(e.getMessage, Some(e)))
  }
}
