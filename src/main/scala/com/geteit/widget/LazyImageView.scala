package com.geteit.widget

import android.content.Context
import android.graphics._
import android.graphics.drawable.{ColorDrawable, Drawable}
import android.net.Uri
import android.util.AttributeSet
import android.widget.ImageView
import android.widget.ImageView.ScaleType
import com.geteit.app.{ViewHelper, GtContext, R}
import com.geteit.concurrent.CancellableFuture.CancelException
import com.geteit.concurrent.{CancellableFuture, Threading}
import com.geteit.events.{Signal, ViewEventContext}
import com.geteit.image.ImageProvider
import com.geteit.util.GtAssert
import com.geteit.util.Log._
import com.geteit.view.GtValueAnimator
import com.geteit.inject.Injectable

import scala.util.{Failure, Success}

/**
 * Simple bitmap drawable always scaling image according to CENTER_CROP.
 */
class SimpleImageDrawable extends Drawable {

  private var _alpha = 255
  private var _image: Bitmap = null

  private val src = new Rect
  private val paint = {
    val p = new Paint
    p.setFilterBitmap(true)
    p
  }

  override def onBoundsChange(bounds: Rect) {
    updateSrc()
  }

  override def getIntrinsicWidth: Int = if (_image != null) _image.getWidth else super.getIntrinsicWidth
  override def getIntrinsicHeight: Int = if (_image != null) _image.getHeight else super.getIntrinsicHeight

  def image = _image
  def image_=(image: Bitmap) {
    _image = image
    updateSrc()
    invalidateSelf()
  }

  private def updateSrc() {
    val bounds = getBounds
    if (_image != null && bounds.bottom > bounds.top) {
      val iw = _image.getWidth
      val ih = _image.getHeight
      val aspect = bounds.width.toFloat / bounds.height
      val w = iw min (ih * aspect).toInt
      val h = ih min (iw / aspect).toInt
      val l = (iw - w) / 2
      val t = (ih - h) / 2
      src.set(l, t, l + w, t + h)
    }
  }

  def draw(canvas: Canvas) {
    if (_image != null) {
      canvas.drawBitmap(_image, src, getBounds, paint)
    }
  }

  def alpha = _alpha
  def setAlpha(alpha: Int) {
    _alpha = alpha
    paint.setAlpha(alpha)
    invalidateSelf()
  }

  def setColorFilter(cf: ColorFilter) {
    paint.setColorFilter(cf)
    invalidateSelf()
  }

  def getOpacity: Int = if (_alpha == 255) PixelFormat.OPAQUE else PixelFormat.TRANSLUCENT
}

class LazyImageView(context: Context, attrs: AttributeSet, style: Int) extends ImageView(context, attrs, style) with ViewHelper {
  def this(context: Context) = this(context, null, 0)
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)

  private implicit val tag: LogTag = "LazyImageView"

  private val provider = inject[ImageProvider]
  private val imageDrawable = new SimpleImageDrawable
  val image = Signal[Bitmap]()

  private var imageUri: Uri = null
  private var task: CancellableFuture[(Bitmap, Boolean)] = CancellableFuture.cancelled()
  private var filter = Option.empty[Bitmap => Bitmap]

  private lazy val loadAnimator = new DrawableLevelAnimator
  private lazy val fadeAnim = new GtValueAnimator onUpdate (imageDrawable.setAlpha(_: Int)) onFinished setBackgroundDrawable(null)

  private var waitingForLoadingStart = false
  private var background: Drawable = null

  // parameters
  private val a = context.obtainStyledAttributes(attrs, R.styleable.LazyImageView)
  private var fadeIn = a.getBoolean(R.styleable.LazyImageView_fadeIn, true)
  private var loadAnimation = a.getBoolean(R.styleable.LazyImageView_loadAnimation, true)
  private val loadResId = a.getResourceId(R.styleable.LazyImageView_loadingDrawable, 0)
  private val errorResId = a.getResourceId(R.styleable.LazyImageView_errorDrawable, 0)
  a.recycle()

  private lazy val loadingDrawable = if (loadResId == 0) new ColorDrawable(Color.TRANSPARENT) else context.getResources.getDrawable(loadResId)
  private lazy val errorDrawable = if (errorResId == 0) new ColorDrawable(Color.TRANSPARENT) else context.getResources.getDrawable(errorResId)

  private var requestedImageSize = 0

  override def onFinishInflate() {
    super.onFinishInflate()

    background = getBackground
  }

  def setFadeIn(fadeIn: Boolean) = {
    this.fadeIn = fadeIn
    this
  }

  def setLoadAnimation(loadAnimation: Boolean) {
    this.loadAnimation = loadAnimation
  }

  override def setImageURI(uri: Uri) {
    setImageURI(uri, resetOld = true)
  }

  def setBitmapFilter(filter: Bitmap => Bitmap): Unit = {
    this.filter = Some(filter)
  }

  def setImageURI(uri: Uri, resetOld: Boolean, forceReload: Boolean = false) {
    if (uri != this.imageUri || forceReload) {
      verbose(s"setImageUri: $uri, previous: $imageUri")

      if (resetOld) reset()

      this.imageUri = uri

      if (loadAnimation) {
        setupLoadingDrawable()
      }

      startLoading()
    }
  }

  private def setupLoadingDrawable() {
    super.setScaleType(ScaleType.CENTER)
    setImageDrawable(loadingDrawable)
    loadAnimator.setDrawable(loadingDrawable)
    loadAnimator.start()
  }

  def setImage(bitmap: Bitmap, immediate: Boolean = false) = {
    image ! bitmap
    imageDrawable.image = bitmap

    if (bitmap == null || bitmap == com.geteit.bitmap.Empty) {
      super.setScaleType(ScaleType.CENTER)
      setImageDrawable(errorDrawable)
    } else {
      super.setScaleType(ScaleType.FIT_XY)
      setImageDrawable(imageDrawable)

      if (fadeIn && !immediate) {
        fadeAnim.startInt(0, 255)
      } else {
        imageDrawable.setAlpha(255)
        setBackgroundDrawable(null)
      }
    }
  }

  override def onLayout(changed: Boolean, left: Int, top: Int, right: Int, bottom: Int) {
    super.onLayout(changed, left, top, right, bottom)

    if (waitingForLoadingStart && getWidth > 0 || imageUri != null && getWidth != requestedImageSize) {
      startLoading()
    }
  }

  private def startLoading() {
    GtAssert.assertUIThread()
    verbose(s"startLoading, uri: $imageUri, w: $getWidth")

    if (getWidth == 0) {
      // wait for first layout
      waitingForLoadingStart = true
      requestLayout()
    } else {
      waitingForLoadingStart = false
      requestedImageSize = getWidth

      if (imageUri != null) {
        if (task != null) {
          warn(s"loading previously started $imageUri")
          task.cancel()
        }

        verbose(s"loading $imageUri $getWidth")

        task = filter.fold(provider(imageUri, getWidth)) { filter =>
          provider(imageUri, getWidth) .map { case (b, i) => (filter(b), false) } (Threading.image)
        }
        task.onComplete {
          case Success((bitmap, immediate)) =>
            verbose(s"imageLoaded uri: $imageUri, image: ${Option(bitmap).map(b => (b.getWidth, b.getHeight))}, view width: $getWidth")
            if (loadAnimator != null) loadAnimator.end()

            task = null
            setImage(bitmap, immediate)
          case Failure(CancelException) => // ignore
          case Failure(ex) =>
            error(s"loading failed $imageUri", ex)
            setImage(null)
        } (Threading.ui)
      }
    }
  }

  def reset() {
    verbose(s"reset, current uri: $imageUri")

    GtAssert.assertUIThread()

    if (task != null) {
      task.cancel()
    }
    task = null

    setImageResource(android.R.color.transparent)
    image ! null

    imageUri = null
    if (loadAnimator != null) {
      loadAnimator.end()
    }

    if (fadeIn) {
      fadeAnim.cancel()
      imageDrawable.setAlpha(255)
    }
    if (background != null) setBackgroundDrawable(background)
  }
}
