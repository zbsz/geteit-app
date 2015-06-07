package com.geteit.net

sealed trait RangeSpec {
  val rangeSpec: String
}
object RangeSpec {
  val Header = "Range"

  def apply(ranges: RangeSpec*) = "Range" -> ranges.map(_.rangeSpec).mkString("bytes=", ",", "")

  case class Range(offset: Long = 0, length: Long = -1) extends RangeSpec {
    require(offset >= 0)
    val rangeSpec = s"$offset-${if (length > 0) (offset + length - 1).toString else ""}"
  }
  case class SuffixRange(offset: Long) extends RangeSpec {
    require(offset >= 0)
    override val rangeSpec: String = s"-$offset"
  }
}

case class ContentRange(from: Long, to: Long, total: Long)

object ContentRange {
  val Header = "Content-Range"
  private val HeaderValue = """bytes (\d+)-(\d+)/(\d+)""".r

  def unapply(header: String): Option[ContentRange] = header match {
    case HeaderValue(from, to, total) => Some(ContentRange(from.toLong, to.toLong, total.toLong))
    case _ => None
  }
}
