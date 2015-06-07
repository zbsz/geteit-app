package com.geteit.bitmap

import android.graphics.Bitmap
import com.geteit.concurrent.{Threading, CancellableFuture}
import com.geteit.util.BoxedError
import com.geteit.util.Log._

object BitmapBlur {
  private implicit val logTag: LogTag = "BitmapBlur"
  import Threading.image
  
  val ComputeCores = Runtime.getRuntime.availableProcessors()
  
  def apply(bitmap: Bitmap, width: Int, radius: Float): CancellableFuture[Bitmap] = {

    def blur(w: Int, retries: Int): CancellableFuture[Bitmap] =
      CancellableFuture { scaledCopy(bitmap, w) } flatMap { dst =>
        val sr = scaledImageBlurRadius(dst.getWidth, width, radius)

        RSBlurProcess(dst, sr).recoverWith {
          case e @ BoxedError(_: OutOfMemoryError) => CancellableFuture.failed(e)
          case e: Throwable => error(s"RS blur failed", e)
            fallback(dst, sr)
        }
      } recoverWith {
        case BoxedError(_: OutOfMemoryError) if retries > 0 && w > 16 =>
          debug(s"blur failed with OOM, will retry, w: $w, retries: $retries")
          blur(w / 2, retries - 1)
      }

    blur(width, 2)
  }

  /**
   * Creates a copy of input bitmap, possibly scaling it to required width.
   * Output image dimensions are always multiply of 8, these images seem to work better with RS
   */
  def scaledCopy(src: Bitmap, width: Int): Bitmap = {

    def shouldScale(width: Int) = {
      val w = src.getWidth / 8 * 8
      val h = src.getHeight / 8 * 8
      w != 0 && h != 0 && (w != src.getWidth || h != src.getHeight || w > width * 1.25f)
    }

    BoxedError.boxOoM {
      if (shouldScale(width)) {
        val w = width / 8 * 8
        val scale = width.toFloat / src.getWidth
        val h = (scale * src.getHeight).toInt / 8 * 8
        Bitmap.createScaledBitmap(src, w, h, false)
      } else {
        src.copy(Bitmap.Config.ARGB_8888, true)
      }
    }
  }

  def scaledImageBlurRadius(imageSize: Int, targetSize: Int, targetRadius: Float): Int = (targetRadius * imageSize / targetSize).toInt

  def fallback(original: Bitmap, radius: Int): CancellableFuture[Bitmap] = {
    val w = original.getWidth
    val h = original.getHeight
    val time = System.currentTimeMillis()

    def getPixels = CancellableFuture {
      BoxedError.boxOoM {
        val pixels = new Array[Int](w * h)
        original.getPixels(pixels, 0, w, 0, 0, w, h)
        pixels
      }
    }

    def horizontal(pixels: Array[Int]) = (0 until ComputeCores) map { core =>
      CancellableFuture {
        JavaBlurProcess.blurIteration(pixels, w, h, radius, ComputeCores, core, 1)
      }
    }

    def vertical(pixels: Array[Int]) = (0 until ComputeCores) map { core =>
      CancellableFuture {
        JavaBlurProcess.blurIteration(pixels, w, h, radius, ComputeCores, core, 2)
      }
    }

    getPixels flatMap { pixels =>
      CancellableFuture.sequence(horizontal(pixels)) flatMap { _ =>
        CancellableFuture.sequence(vertical(pixels)) map { _ =>
          original.setPixels(pixels, 0, w, 0, 0, w, h)
          original
        }
      }
    } map { res =>
      verbose(s"Fallback java blur (${original.getWidth}, $radius) time: ${System.currentTimeMillis() - time} ms")
      res
    }
  }
}
