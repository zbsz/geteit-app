package com.geteit.widget

import android.graphics.drawable.Drawable
import android.view.animation.LinearInterpolator
import com.geteit.app.GtContext
import com.geteit.events.EventContext
import com.nineoldandroids.animation.ValueAnimator
import com.nineoldandroids.animation.ValueAnimator.AnimatorUpdateListener

object DrawableLevelAnimator {
    
    val INTERPOLATOR = new LinearInterpolator()
}

class DrawableLevelAnimator(duration: Long = 2000)(implicit context: GtContext, eventContext: EventContext) extends ValueAnimator with AnimatorUpdateListener {
    
    var drawable: Drawable = null
    
    var step = 200
    var prevLevel = -1

    setInterpolator(DrawableLevelAnimator.INTERPOLATOR)
    setFloatValues(0, 1)
    setRepeatCount(ValueAnimator.INFINITE)
    setDuration(duration)

    addUpdateListener(this)

    context.ctxDestroyed(if (_) {
        end()
        drawable = null
    })
    
    def setDrawable(drawable: Drawable) {
        this.drawable = drawable
    }

    def start(drawable: Drawable) {
        this.drawable = drawable
        start()
    }

    def setStep(step: Int) {
        this.step = step
    }

    def onAnimationUpdate(animation: ValueAnimator) {
        if (drawable != null) {
            val level = (animation.getAnimatedFraction * 10000).toInt / step * step
            if (prevLevel != level) {
                prevLevel = level
                drawable.setLevel(level)
            }
        }
    }
}
