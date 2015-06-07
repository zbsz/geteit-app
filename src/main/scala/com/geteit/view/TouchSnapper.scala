package com.geteit.view

class TouchSnapper(radius: Float = 32) extends Point(0, 0) {

  private val tmp = new Point(0, 0)
  private val start = new Point(0, 0)
  private var started = false
  private var snapping = true

  def reset() = {
    started = false
    snapping = true
  }

  def update(x: Float, y: Float): Point = {
    if (!started) {
      started = true
      start.set(x, y)
      this.x = x
      this.y = y
    } else if (snapping) {
      val d = start.dst(x, y)
      tmp.set(x - start.x, y - start.y)
      if (d < radius) {
        tmp.mul(d / (radius * 2))
      }
      else if (d < 2 * radius) {
        tmp.mul((3 * d / 2 - radius) / d)
      }
      else {
        snapping = false
      }
      this.x = start.x + tmp.x
      this.y = start.y + tmp.y
    } else {
      this.x = x
      this.y = y
    }

    this
  }

  def update(position: Point): Point = update(position.x, position.y)
}