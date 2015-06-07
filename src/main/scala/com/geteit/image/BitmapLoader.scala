package com.geteit.image

import java.io.{File, FileInputStream, InputStream}

import android.app.ActivityManager
import android.content.{ContentResolver, Context}
import android.graphics.{Bitmap, BitmapFactory}
import android.net.Uri
import android.provider.MediaStore
import android.provider.MediaStore.Images.ImageColumns
import android.util.Log
import com.geteit.app.GtContext
import com.geteit.util.IoUtils
import com.geteit.util.Log._

/**
  */
object BitmapLoader {

    private implicit val tag: LogTag = "BitmapUtils"
    private val UNCONSTRAINED: Int = -1

    lazy val MAX_SIZE = {
        val memoryClass = GtContext.Global.getSystemService(Context.ACTIVITY_SERVICE).asInstanceOf[ActivityManager].getMemoryClass
        verbose(s"memoryClass: $memoryClass")

        (1 + (memoryClass - 16) / 2) * 1024 * 1024 / 16  // max number of pixels in loaded image (1024x1024 on base android devices)
    }

    private lazy val contentResolver = GtContext.Global.getContentResolver

    def apply(data: Array[Byte]): Option[Bitmap] = apply(data, UNCONSTRAINED, MAX_SIZE)
    def apply(imageData: Array[Byte], minSideLen: Int, maxSize: Int): Option[Bitmap] = {
        val opts = new BitmapFactory.Options
        opts.inJustDecodeBounds = true
        BitmapFactory.decodeByteArray(imageData, 0, imageData.length, opts)
        apply(imageData, BitmapUtils.computeSampleSize(opts, minSideLen, maxSize))
    }

    def apply(file: File): Option[Bitmap] = apply(file, UNCONSTRAINED, MAX_SIZE)
    def apply(file: File, minSideLen: Int, maxSize: Int): Option[Bitmap] =
        inSampleSize(new FileInputStream(file), minSideLen, maxSize) flatMap { apply(new FileInputStream(file), _) }

    def apply(uri: Uri): Option[Bitmap] = apply(uri, UNCONSTRAINED, MAX_SIZE)
    def apply(uri: Uri, minSideLen: Int, maxSize: Int): Option[Bitmap] = {

        val result = inSampleSize(contentResolver.openInputStream(uri), minSideLen, MAX_SIZE) flatMap { apply(contentResolver.openInputStream(uri), _) }

        val rotation = imageRotation(uri)
        if (rotation == 0) result else result map { BitmapUtils.rotate(_, rotation) }
    }

    /**
     * Loads image rotation on degrees from media uri.
     */
    private def imageRotation(uri: Uri) = {
        if (isMediaUri(uri)) {
            val c = contentResolver.query(uri, null, null, null, null)
            if (c != null && c.moveToFirst) {
                val index = c.getColumnIndex(ImageColumns.ORIENTATION)
                if (index < 0) 0 else c.getInt(index)
            } else 0
        } else 0
    }

    private def isMediaUri(uri: Uri) = uri != null && ContentResolver.SCHEME_CONTENT == uri.getScheme && MediaStore.AUTHORITY == uri.getAuthority

    private def inSampleSize(f: => InputStream, minSideLen: Int, maxSize: Int) = {
        var is: InputStream = null
        try {
            is = f
            val opts = new BitmapFactory.Options
            opts.inJustDecodeBounds = true
            BitmapFactory.decodeStream(is, null, opts)
            Some(BitmapUtils.computeSampleSize(opts, minSideLen, maxSize))
        } catch {
            case e: Throwable =>
                error("error decoding bounds", e)
                None
        }
    }

    def apply(data: Array[Byte], inSampleSize: Int): Option[Bitmap] = apply(BitmapFactory.decodeByteArray(data, 0, data.length, _), inSampleSize)

    def apply(inputStream: => InputStream, inSampleSize: Int): Option[Bitmap] = apply({ opts =>
        IoUtils.withResource(inputStream) { is =>
            BitmapFactory.decodeStream(is, null, opts)
        }
    }, inSampleSize)

    private def apply(f: BitmapFactory.Options => Bitmap, inSampleSize: Int): Option[Bitmap] = try {
        val opts = new BitmapFactory.Options
        opts.inDither = false
        opts.inSampleSize = inSampleSize
        opts.inPreferredConfig = Bitmap.Config.ARGB_8888
        Option(f(opts))
    } catch {
        case e: Throwable =>
            Log.e("LoadImage", e.getMessage, e)
            None
    }
}
