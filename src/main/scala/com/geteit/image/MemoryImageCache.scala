package com.geteit.image

import android.app.ActivityManager
import android.graphics.Bitmap
import android.net.Uri
import com.geteit.events.{EventContext, EventStream}
import com.geteit.util.Log._
import com.geteit.util.{GtAssert, LruCache}
import com.geteit.inject.{Injectable, Injector}

import scala.collection.mutable

class MemoryImageCache(implicit injector: Injector) extends Injectable {

    private implicit val tag: LogTag = "MemoryImageCache"
    private implicit val eventContext = inject[EventContext]

    val onInvalidated = new EventStream[Uri]

    private val cache = new LruCache[String, Bitmap]((inject[ActivityManager].getMemoryClass - 4) * 1024 * 1024 / 6) { // 2Mb on base android devices (with 16Mb ram limit)
        override def sizeOf(key: String, value: Bitmap) = value.getRowBytes * value.getHeight
    }

    private val imageKeys = new mutable.HashMap[Uri, mutable.HashSet[String]]

    def put(imageUri: Uri, key: String, image: Bitmap) = {
        GtAssert(image != null)
        GtAssert(image != BitmapUtils.EMPTY_BITMAP)

        imageKeys.getOrElseUpdate(imageUri, new mutable.HashSet[String]) add key

        cache.put(key, image)
    }

    def apply(key: String) = Option(cache.get(key))

    def apply(uri: Uri) = Option(cache.get(uri.toString))

    def apply(key: String, f: => Option[Bitmap]) = Option(cache.get(key)).orElse {
        f match {
            case Some(image) => cache.put(key, image); Some(image)
            case None => None
        }
    }

    def apply(uri: Uri, key: String, f: => Option[Bitmap]) = Option(cache.get(key)).orElse {
        f match {
            case Some(image) => put(uri, key, image); Some(image)
            case None => None
        }
    }

    def update(uri: Uri, image: Bitmap) = put(uri, uri.toString, image)

    def update(key: String, image: Bitmap) = cache.put(key, image)

    def get(key: String) = cache.get(key)
    
    def invalidate(key: String) = cache.remove(key)
    
    def invalidate(imageUri: Uri) {
        verbose(s"invalidate $imageUri")

        imageKeys.remove(imageUri).foreach(_.foreach { key =>
            verbose(s"invalidate: $key")
            cache.remove(key)
        })

        onInvalidated ! imageUri
    }

    def getBitmap(url: String): Bitmap = get(url)

    def putBitmap(url: String, bitmap: Bitmap) {
        cache.put(url, bitmap)
    }
}
