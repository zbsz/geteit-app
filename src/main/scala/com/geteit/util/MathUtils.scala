package com.geteit.util

import com.geteit.view.Point

object MathUtils {

  val PI: Float = Math.PI.toFloat
  val TWO_PI: Float = 2 * PI
  val PI_OVER_2: Float = PI / 2
  val EPS: Float = 1e-4f

  def normalizeAngle(angle: Float): Float = {
    var res = angle % TWO_PI
    if (res < 0) {
      res += TWO_PI
    }
    res
  }

  def nextPowerOfTwo(value: Int): Int = {
    if (value == 0) 1
    else {
      var res = value - 1
      res |= res >> 1
      res |= res >> 2
      res |= res >> 4
      res |= res >> 8
      res |= res >> 16
      res + 1
    }
  }

  def clamp(v: Float, min: Float, max: Float) = if (v < min) min else if (v > max) max else v

  def ceil(v: Float) = Math.ceil(v).toInt

  /**
   * @return the angle in radians of this vector (point) relative to the x-axis. Angles are counter-clockwise and between 0 and 2PI
   */
  def getAngle(v: Point): Float = {
    var angle: Float = Math.atan2(v.y, v.x).toFloat
    if (angle < 0) angle += TWO_PI
    angle
  }

  /**
   * @param angle - angle in radians
   * @return
   */
  def getVector(angle: Float, out: Point): Point = {
    out.set(Math.cos(angle).toFloat, Math.sin(angle).toFloat)
    out
  }

  def angleDiff(angle1: Float, angle2: Float): Float = {
    var result: Float = Math.abs((angle1 - angle2) % TWO_PI)
    if (result > PI) {
      result = TWO_PI - result
    }
    result
  }

  def toDeg(rad: Float): Float = rad * 180 / PI
  def toRad(deg: Float): Float = deg * PI / 180

  def log2(value: Int): Int = {
    var v = value
    var res: Int = 0
    while ((v & 0x1) == 0 || (v & 0xFFFFFFFe) != 0) {
      v = v >> 1
      res += 1
    }
    res
  }
}
