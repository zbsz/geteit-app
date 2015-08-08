package com.geteit.view

import android.content.Context
import android.os.{Looper, SystemClock}
import android.view.View
import android.view.animation.LinearInterpolator
import com.geteit.util.{GtObjHandler, MathUtils}
import com.nineoldandroids.animation.Animator.AnimatorListener
import com.nineoldandroids.animation.ValueAnimator.AnimatorUpdateListener
import com.nineoldandroids.animation.{ValueAnimator, AnimatorListenerAdapter, Animator}
import com.geteit.util.Log._

object GtValueAnimator {
}

class GtValueAnimator(duration: Long = 300) {

    private var updateListener: Option[AnimatorUpdateListener] = None
    private var animListener: Option[AnimatorListener] = None

    protected lazy val anim = {
        val a = new ValueAnimator
        a.setDuration(duration)
        updateListener.foreach(a.addUpdateListener)
        animListener.foreach(a.addListener)
        a
    }
    
    def onFinished(f: => Unit) = {
        animListener = Some(new AnimatorListenerAdapter() {
            override def onAnimationEnd(anim: Animator) {
                f
            }
        })
        this
    }

    def onUpdate[T <: AnyVal](f: T => Unit) = {
        updateListener = Some(new AnimatorUpdateListener() {
            def onAnimationUpdate(anim: ValueAnimator) {
                f(anim.getAnimatedValue.asInstanceOf[T])
            }
        })
        this
    }

    def start(values: Float*) = {
//        GtAssert.assertUIThread()

        if (values.nonEmpty) anim.setFloatValues(values: _*)
        anim.start()
        this
    }

    def startInt(start: Int, end: Int) = {
//        GtAssert.assertUIThread()

        anim.setIntValues(start, end)
        anim.start()
        this
    }

    def updateInt(start: Int, end: Int) = {
        if (anim.isRunning) {
          val value = anim.getAnimatedValue.asInstanceOf[Int]
          val values = anim.getValues
        } else {
          anim.setIntValues(start, end)
          anim.start()
        }
        this
    }

    def end() {
//        GtAssert.assertUIThread()

        if (anim.isRunning) anim.end()
    }
    def cancel() {
//        GtAssert.assertUIThread()

        if (anim.isRunning) anim.cancel()
    }
    
    def isRunning = anim.isRunning
}

class ContinuousValueAnimator(var start: Long, onUpdate: Long => Unit) extends AnimatorUpdateListener {
  private implicit val tag: LogTag = "ContinuousValueAnimator"

  lazy val anim = {
    val a = new ValueAnimator
    a.addUpdateListener(this)
    a.setInterpolator(new LinearInterpolator())
    a.setFloatValues(0f, 1f)
    a
  }

  var current: Long = start
  var target: Long = 0

  def cancel() = if (anim.isRunning) anim.cancel()

  def set(value: Long) = {
    cancel()
    current = value
    onUpdate(current)
  }

  def animateTo(target: Long) = {
    this.target = target
    if (anim.isRunning) {
      val duration = anim.getDuration
      val shouldContinue = (current > start && target >= current) || (current < start && target <= current)
      if (shouldContinue) {
        val time = ((current - start).toFloat / (target - start) * duration).toLong
        anim.setCurrentPlayTime(time)
      } else {
        anim.cancel()
        start = current
        anim.start()
      }
    } else {
      start = current
      anim.start()
    }
  }

  override def onAnimationUpdate(animation: ValueAnimator): Unit = {
    current = start + ((target - start) * animation.getAnimatedFraction + .5).toLong
    onUpdate(current)
  }
}

class FadeAnimator(view: View, duration: Long = 300, hiddenVisibility: Int = View.GONE) extends GtValueAnimator(duration) {
    
    private val transform = ViewTransformFactory.createTransform(view)
    var fadingOut = false
    
    onUpdate(transform.setAlpha(_: Float))
    
    onFinished {
        if (fadingOut) view.setVisibility(hiddenVisibility)
    }

    def cancel(visible: Boolean) {
        cancel()
        transform.setAlpha(1f)
        if (visible) transform.setVisibility(View.VISIBLE) else transform.setVisibility(hiddenVisibility)
    }

    def fadeIn() {
        end()
        if (view.getVisibility != View.VISIBLE) {
            transform.setAlpha(0)
        }
        if (transform.getAlpha < 1) {
            transform.setVisibility(View.VISIBLE)
            fadingOut = false
            start(transform.getAlpha, 1)
        }
    }
    
    def fadeOut() {
        end()
        if (transform.getAlpha > 0) {
            fadingOut = true
            start(transform.getAlpha, 0)
        }
    }
}


object OverScrollerAnimator {
    val scrollHandler = new GtObjHandler({scroller: OverScrollerAnimator =>
        if (scroller._onScroll != null) scroller._onScroll(scroller.position.x.toInt, scroller.position.y.toInt)
    }, Looper.getMainLooper)

    val TAG = "OverScrollerAnimator"
}

class OverScrollerAnimator(context: Context, duration: Long = 300) extends GtValueAnimator(10000000) {

    import OverScrollerAnimator._

    private var _onScroll : (Int, Int) => Any = null
    private val _scroller = new OverScroller(context)
    private var ending = false
    private var velocityScrolling = false
    
    private var minX = 0
    private var minY = 0
    private var maxX = 0
    private var maxY = 0
    private var overX = 0
    private var overY = 0
    private val velocity = new Point(0, 0)
    private val position = new Point(0, 0)
    private var lastTime = 0L
    
    onUpdate { _: Float =>
        if (velocityScrolling) {
            // TODO: think about using overscroll and springback here
            val dt = SystemClock.uptimeMillis() - lastTime
            position.set(MathUtils.clamp(position.x + velocity.x * dt / 1000, minX, maxX), MathUtils.clamp(position.y + velocity.y * dt / 1000, minY, maxY))
            if (_onScroll != null) scrollHandler ! this
            lastTime = SystemClock.uptimeMillis()
        } else {
            if (!_scroller.isFinished) {
                _scroller.computeScrollOffset()
                if (_onScroll != null) {
                    position.set(_scroller.getCurrX, _scroller.getCurrY)
                    scrollHandler ! this
                }
            }
            
            if (_scroller.isFinished && !ending) {
                ending = true
                end()
                ending = false
            }
        }
    }
    
    def onScroll(f: (Int, Int) => Any) = { 
        _onScroll = f
        this
    }
    
    def startScroll(startX: Int, startY: Int, dx: Int, dy: Int, duration: Long = this.duration) = {
        velocityScrolling = false
        if (dx != 0 || dy != 0) {
            _scroller.startScroll(startX, startY, dx, dy, duration.toInt)
            start(0, 1)
        }
    }

    def scrollTo(startX: Int, startY: Int, endX: Int, endY: Int, duration: Long = this.duration) {
        startScroll(startX, startY, endX - startX, endY - startY, duration)
    }

    def fling(startX: Int, startY: Int, velocityX: Int, velocityY: Int, minX: Int, maxX: Int, minY: Int, maxY: Int, overX: Int, overY: Int): OverScrollerAnimator = {
        velocityScrolling = false
        _scroller.fling(startX, startY, velocityX, velocityY, minX, maxX, minY, maxY, overX, overY)
        start(0, 1)
        this
    }
    
    def fling(startX: Int, startY: Int, velocityX: Int, velocityY: Int): OverScrollerAnimator = 
            fling(startX, startY, velocityX, velocityY, minX, maxX, minY, maxY, overX, overY)
    
    def springBack(startX: Int, startY: Int, minX: Int, maxX: Int, minY: Int, maxY: Int): OverScrollerAnimator = {
        velocityScrolling = false
        if (_scroller.springBack(startX, startY, minX, maxX, minY, maxY)) {
            start(0, 1)
        }
        this
    }
    
    def springBack(startX: Int, startY: Int): OverScrollerAnimator = springBack(startX, startY, minX, maxX, minY, maxY)
        
    def setOverscroll(overX: Int, overY: Int) = {
        this.overX = overX
        this.overY = overY
        this
    }
    
    def setRange(minX: Int, maxX: Int, minY: Int, maxY: Int) = {
        this.minX = minX
        this.maxX = maxX
        this.minY = minY
        this.maxY = maxY
        this
    }
    
    def startScrolling(startX: Int, startY: Int, velocityX: Float, velocityY: Float) = {
        lastTime = SystemClock.uptimeMillis()
        velocityScrolling = true
        position.set(startX, startY)
        velocity.set(velocityX, velocityY)
        start(0, 1)
        this
    }
    
    def setVelocity(velocityX: Float, velocityY: Float) = {
        velocity.set(velocityX, velocityY)
        this
    }
    
    def setFriction(friction: Float) = {
        _scroller.setFriction(friction)
        this
    }
    
    override def cancel() {
        velocityScrolling = false
        _scroller.abortAnimation()
        scrollHandler.removeMessages(this)
        super.cancel()
    }

    override def end() {
        velocityScrolling = false
        scrollHandler.removeMessages(this)
        super.end()
    }
}
