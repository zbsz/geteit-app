package com.geteit.bitmap

import android.graphics.Bitmap
import android.support.v8.renderscript.{Element, Allocation}
import com.geteit.app.{GtApplication, R}
import com.geteit.util.Log._

object RSBlurProcess {

  private implicit val logTag: LogTag = "RSBlurProcess"

  lazy val blurScript = new ScriptC_blur(RenderScriptContext.rs, GtApplication.APP_INSTANCE.getResources, R.raw.blur)

  /**
   * Performs blur operation on given image using renderscript implementation.
   * This is destructive operation, passed bitmap will be overwritten.
   *
   * @param image - image to blur, will be overwriten
   * @param radius - blur radius
   * @return image - blurred image, this is the same instance as incoming one
   */
  def apply(image: Bitmap, radius: Int) = RenderScriptContext { rs =>
    logTime(s"RS blurring (${image.getWidth}, ${image.getHeight}) r: $radius") {
      // XXX: renderscript allocation doesn't work for some bitmap sizes, multiply of 8 seems to be safe, so we will crop initial bitmap sometimes
      val width = image.getWidth
      val height = image.getHeight
      val inAllocation = Allocation.createFromBitmap(rs, image, Allocation.MipmapControl.MIPMAP_NONE, Allocation.USAGE_SCRIPT)
      val rows = Allocation.createSized(rs, Element.U32(rs), height, Allocation.USAGE_SCRIPT)
      val columns = Allocation.createSized(rs, Element.U32(rs), width, Allocation.USAGE_SCRIPT)
      rows.copyFrom(Array.tabulate(height)(x => x))
      columns.copyFrom(Array.tabulate(width)(x => x))

      blurScript.set_gIn(inAllocation)
      blurScript.set_width(width)
      blurScript.set_height(height)
      blurScript.set_radius(radius)
      blurScript.forEach_blur_h(rows)
      blurScript.forEach_blur_v(columns)

      inAllocation.copyTo(image)
      inAllocation.destroy()
      rows.destroy()
      columns.destroy()
      image
    }
  }
}
