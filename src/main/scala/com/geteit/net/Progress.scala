package com.geteit.net

case class Progress(current: Long, range: ContentRange, state: Progress.State)

object Progress {
  val Empty = Progress(0, ContentRange(0, 0, 0), Waiting)

  sealed trait State
  case object Waiting extends State
  case object Running extends State
  case class Failed(cause: Throwable) extends State
  case object Cancelled extends State
  case object Done extends State
}
