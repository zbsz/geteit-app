package com.geteit.view

import android.content.Context
import android.os.SystemClock
import android.support.v4.graphics.drawable.DrawableCompat
import android.view.View.OnTouchListener
import android.view.{MotionEvent, VelocityTracker, View, ViewConfiguration}
import com.geteit.concurrent.{CancellableFuture, Threading}
import com.geteit.events._
import com.geteit.util._

import scala.concurrent.duration._

class TouchReactor(val view: View) { reactor =>

  val onDown = new Publisher[MotionEvent]
  val onUp = new Publisher[MotionEvent]
  val onCancel = new Publisher[MotionEvent]
  val onDrag = new Publisher[MotionEvent]

  def onTouch(ev: MotionEvent) = {
    ev.getAction match {
      case MotionEvent.ACTION_DOWN => onDown ! ev
      case MotionEvent.ACTION_MOVE => onDrag ! ev
      case MotionEvent.ACTION_UP =>
        onUp ! ev
        onDrag.unsubscribeAll()
        onUp.unsubscribeAll()
        onCancel.unsubscribeAll()
      case MotionEvent.ACTION_CANCEL =>
        onCancel ! ev
        onDrag.unsubscribeAll()
        onUp.unsubscribeAll()
        onCancel.unsubscribeAll()
      case _ => //ignore
    }

    onUp.hasSubscribers || onDrag.hasSubscribers || onCancel.hasSubscribers
  }

  view.setOnTouchListener(new OnTouchListener {
    override def onTouch(v: View, event: MotionEvent): Boolean = reactor.onTouch(event)
  })
}

trait PressStateUpdater extends TouchReactor {
  private var pressed = false
  private var pressStart = 0L
  private def unpressDelay = ViewConfiguration.getPressedStateDuration min (SystemClock.uptimeMillis() - pressStart).toInt

  override def onTouch(ev: MotionEvent) = {
    val res = super.onTouch(ev)
    ev.getAction match {
      case MotionEvent.ACTION_DOWN =>
        if (res) {
          Option(view.getBackground) foreach { DrawableCompat.setHotspot(_, ev.getX, ev.getY) }
          view.setPressed(true)
          pressed = true
          pressStart = SystemClock.uptimeMillis()
        }
      case MotionEvent.ACTION_MOVE =>
        Option(view.getBackground) foreach { DrawableCompat.setHotspot(_, ev.getX, ev.getY) }
      case MotionEvent.ACTION_UP =>
        pressed = false
        view.postDelayed(view.setPressed(pressed), unpressDelay)
      case MotionEvent.ACTION_CANCEL =>
        pressed = false
        view.setPressed(false)
      case _ => //ignore
    }
    res
  }
}

class Point(var x: Float, var y: Float) {
  def set(x: Float, y: Float) = {
    this.x = x
    this.y = y
    this
  }

  def mul(s: Float) = {
    x *= s
    y *= s
    this
  }

  def dst(x: Float, y: Float) = math.sqrt((x - this.x) * (x - this.x) + (y - this.y) * (y - this.y)).toFloat
}

class ClickGesture(context: Context, reactor: TouchReactor, longClickTimeout: FiniteDuration = 500.millis)(implicit eventContext: EventContext) {
  val pressed = Signal[Boolean](false)
  val onClick = new Publisher[(Float, Float)]
  val onLongClick = new Publisher[(Float, Float)]

  var longClickFilter = { (x: Float, y: Float) => true }

  private val touchSlop = ViewConfiguration.get(context.getApplicationContext).getScaledTouchSlop
  private var prev: Cancellable = null

  reactor.onDown { ev: MotionEvent =>
    if (prev != null) prev.cancel()

    val longClick = onLongClick.hasSubscribers && longClickFilter(ev.getX, ev.getY)

    if (onClick.hasSubscribers || longClick) {
      val startPos = new Point(ev.getX, ev.getY)
      pressed ! true

      Cancellable { c =>
        prev = c

        c.onCancel { _ => pressed ! false }

        c ::= reactor.onUp { ev: MotionEvent =>
          onClick !(ev.getX, ev.getY)
          c.cancel()
        }

        c ::= reactor.onCancel { _ => c.cancel() }

        c ::= reactor.onDrag { ev: MotionEvent =>
          if (startPos.dst(ev.getX, ev.getY) > touchSlop) {
            c.cancel()
          }
        }

        if (longClick) {
          CancellableFuture.delayed(longClickTimeout) {
            if (!c.cancelled) {
              reactor.onDrag.unsubscribeAll()
              reactor.onUp.unsubscribeAll()
              onLongClick !(startPos.x, startPos.y)
              c.cancel()
            }
          } (Threading.ui)
        }
      }
    }
  }
}

object DragGesture {
  val NONE = 0
  val HORIZONTAL = 1
  val VERTICAL = 2

  private def isHorizontal(dir: Int) = dir == NONE || dir == HORIZONTAL

  private def isVertical(dir: Int) = dir == NONE || dir == VERTICAL
}

class DragGesture(context: Context, reactor: TouchReactor, direction: Int = DragGesture.NONE)(implicit eventContext: EventContext) {

  import DragGesture._

  val onDragStart = EventStream[(Float, Float)]()
  val onDragEnd = EventStream[Boolean]()
  val onDrag = EventStream[(Float, Float)]()
  val onFling = EventStream[(Float, Float)]()
  var startPos = new Point(0, 0)
  var dragging = false

  private val configuration = ViewConfiguration.get(context.getApplicationContext)
  private val touchSlop = configuration.getScaledTouchSlop
  private val minimumVelocity = configuration.getScaledMinimumFlingVelocity
  private val maximumVelocity = configuration.getScaledMaximumFlingVelocity

  reactor.onDown { ev: MotionEvent =>
    dragging = false

    def stopped(dragged: Boolean) = {
      dragging = false
      onDragEnd ! dragged
    }

    if (onDragStart.hasSubscribers || onDrag.hasSubscribers || onFling.hasSubscribers) {
      startPos.set(ev.getX, ev.getY)
      var velocityTracker: VelocityTracker = null
      var dragObserver: Subscription = null

      def shouldStartDrag(x: Float, y: Float) = {
        direction match {
          case NONE => startPos.dst(ev.getX, ev.getY) > touchSlop
          case VERTICAL => math.abs(ev.getY - startPos.y) > touchSlop && math.abs((ev.getY - startPos.y) / (ev.getX - startPos.x)) > 2 || math.abs(ev.getY - startPos.y) > 2 * touchSlop
          case HORIZONTAL => math.abs(ev.getX - startPos.x) > touchSlop && math.abs((ev.getX - startPos.x) / (ev.getY - startPos.y)) > 2 || math.abs(ev.getX - startPos.x) > 2 * touchSlop
        }
      }

      dragObserver = reactor.onDrag { ev: MotionEvent =>
        if (!dragging && shouldStartDrag(ev.getX, ev.getY)) {
          onDragStart !(ev.getX, ev.getY)

          if (onDrag.hasSubscribers || onFling.hasSubscribers || onDragEnd.hasSubscribers) {
            dragging = true
            startPos.set(ev.getX, ev.getY)
          }

          if (onFling.hasSubscribers) {
            velocityTracker = VelocityTracker.obtain()

            reactor.onUp { ev: MotionEvent =>
              if (velocityTracker != null) {
                velocityTracker.addMovement(ev)
                velocityTracker.computeCurrentVelocity(1000, maximumVelocity)
                if (isHorizontal(direction) && Math.abs(velocityTracker.getXVelocity) > minimumVelocity
                  || isVertical(direction) && math.abs(velocityTracker.getYVelocity) > minimumVelocity) {

                  onFling !(velocityTracker.getXVelocity, velocityTracker.getYVelocity)
                  stopped(true)
                } else {
                  stopped(false)
                }
                velocityTracker.recycle()
                velocityTracker = null
              } else {
                stopped(false)
              }
            }

            reactor.onCancel { _ =>
              if (velocityTracker != null) {
                velocityTracker.recycle()
                velocityTracker = null
              }
              stopped(false)
            }
          } else {
            reactor.onUp { ev: MotionEvent => stopped(true) }
            reactor.onCancel { _ => stopped(false) }
          }
        } else if (!dragging && startPos.dst(ev.getX, ev.getY) > 2 * touchSlop) {
          dragObserver.destroy()
        }
        if (dragging) {
          onDrag !(ev.getX, ev.getY)
        }
        if (velocityTracker != null) {
          velocityTracker.addMovement(ev)
        }
      }
    }
  }
}


