package com.geteit.image

import java.io.IOException

import android.content.ContentResolver
import android.graphics.Bitmap
import android.net.Uri
import com.geteit.bitmap
import com.geteit.cache.{CacheEntry, CacheService}
import com.geteit.concurrent.{CancellableFuture, Threading}
import com.geteit.net.Response.SuccessHttpStatus
import com.geteit.net._
import com.geteit.util.Log._
import com.geteit.util.Serialized
import com.geteit.inject.{Injectable, Injector}

import scala.util.control.NoStackTrace

trait ImageProvider {
  def apply(uri: Uri, size: Int): CancellableFuture[(Bitmap, Boolean)]
}

class BasicImageProvider(implicit injector: Injector) extends ImageProvider with Injectable {
  private implicit val tag: LogTag = "ImageProvider"
  import Threading.image

  private val cache = inject[CacheService]
  private val memoryCache = inject[MemoryImageCache]
  private val client = inject[AsyncClient]

  private val loadingException = new IOException("Image loading failed") with NoStackTrace

  override def apply(uri: Uri, preferredSize: Int = -1): CancellableFuture[(Bitmap, Boolean)] = Serialized(("ImageProvider.apply", uri)) {

    def loadCached(entry: CacheEntry) = CancellableFuture {
      val res = entry.get match {
        case Left(file) => BitmapLoader(file, preferredSize, BitmapLoader.MAX_SIZE)
        case Right(data) => BitmapLoader(data, preferredSize, BitmapLoader.MAX_SIZE)
      }
      if (res.isEmpty || res.contains(bitmap.Empty)) cache.remove(uri.toString)
      res
    }

    def resultFromCache(entry: CacheEntry) = loadCached(entry) flatMap {
      case Some(bitmap) if bitmap != null && bitmap != com.geteit.bitmap.Empty => CancellableFuture.successful(bitmap)
      case None => CancellableFuture.failed(loadingException)
    }

    memoryCache(uri.toString) match {
      case Some(im) => CancellableFuture.successful((im, true))
      case None =>
        (uri.getScheme match {
          case ContentResolver.SCHEME_CONTENT =>
            CancellableFuture.lift(cache.addStream(uri.toString, inject[ContentResolver].openInputStream(uri))) flatMap resultFromCache
          case "http" | "https" =>
            CancellableFuture.lift(cache.getEntry(uri.toString)) flatMap {
              case Some(entry) => loadCached(entry) flatMap {
                  case Some(bitmap) => CancellableFuture.successful(bitmap)
                  case None => download(uri) flatMap resultFromCache
                }
              case None => download(uri) flatMap resultFromCache
            }
          case _ => CancellableFuture { BitmapLoader(uri, preferredSize, BitmapLoader.MAX_SIZE) } flatMap {
            case Some(bitmap) => CancellableFuture.successful(bitmap)
            case None => CancellableFuture.failed(loadingException)
          }
        }) map {
          case im =>
            memoryCache.update(uri, im)
            (im, false)
        }
    }
  }

  def download(uri: Uri) = client(Request.Get(uri)) flatMap {
    case Response(SuccessHttpStatus(), FileResponse(file, _), _) => CancellableFuture.lift(cache.addFile(uri.toString, file, moveFile = true))
    case Response(SuccessHttpStatus(), BinaryResponse(data, _), _) => CancellableFuture.successful(cache.addData(uri.toString, data))
    case resp =>
      error(s"unexpected response for uri: $uri")
      CancellableFuture.failed(new Exception(s"unexpected response: $resp for $uri"))
  }
}
