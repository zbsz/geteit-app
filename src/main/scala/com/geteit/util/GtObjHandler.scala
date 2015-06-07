package com.geteit.util

import android.os.{Message, Handler, Looper}
import com.geteit.util.Log._

class GtObjHandler[T](f: T => Any, looper: Looper = Looper.getMainLooper) extends Handler(looper) {
  private implicit val tag: LogTag = "GtObjHandler"

  override def handleMessage(msg : Message) {
    LoggedTry {
      f(msg.obj.asInstanceOf[T])
    }
  }
  def !(obj: T) {
    obtainMessage(0, obj).sendToTarget()
  }
  def !!(obj: T) {
    removeMessages(obj)
    obtainMessage(0, obj).sendToTarget()
  }
  def apply(f: => Any) {
    post(new Runnable() { def run() { f }})
  }

  def send(obj: T, delay: Long = 0) {
    if (delay > 0) sendMessageDelayed(obtainMessage(0, obj), delay)
    else obtainMessage(0, obj).sendToTarget()
  }
  def removeAllMessages() {
    removeMessages(0)
  }
  def removeMessages(obj: T) {
    removeMessages(0, obj)
  }
  def hasMessages: Boolean = hasMessages(0)
}
