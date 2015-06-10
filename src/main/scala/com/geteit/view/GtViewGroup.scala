package com.geteit.view

import android.view.View.MeasureSpec
import android.view.{ViewGroup, View}
import com.geteit.util.GtObjHandler

trait GtViewGroup extends Seq[View] {
  self: ViewGroup =>

  def length = getChildCount
  def apply(i: Int) = getChildAt(i)
  def iterator = new Iterator[View] {
    var index = 0
    def hasNext = index < getChildCount
    def next() = {
      val v = getChildAt(index)
      index += 1
      v
    }
  }

  def relayout() {
    forceLayout()
    GtViewGroup.relayoutHandler !! this
  }
}

object GtViewGroup {
  lazy val relayoutHandler = new GtObjHandler({ (view: View) =>
    view.measure(MeasureSpec.makeMeasureSpec(view.getWidth, MeasureSpec.EXACTLY), MeasureSpec.makeMeasureSpec(view.getHeight, MeasureSpec.EXACTLY))
    view.layout(view.getLeft, view.getTop, view.getRight, view.getBottom)
  })
}