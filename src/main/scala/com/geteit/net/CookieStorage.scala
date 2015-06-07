package com.geteit.net

import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone}

import android.net.Uri
import com.geteit.concurrent.LimitedExecutionContext
import com.geteit.inject.Factory
import com.geteit.json.Json
import com.geteit.net.CookieStorage.{Cookie, CookieSet}
import com.geteit.util.Log._
import com.geteit.util._
import com.koushikdutta.async.http.Headers

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Try

trait CookieStorage {
  def header(uri: Uri): Future[Option[(String, String)]]
  def update(uri: Uri, headers: Headers): Unit
}

class MemoryCookieStorage extends CookieStorage {
  private implicit val tag: LogTag = "CookieStorage"
  private implicit val dispatcher = new LimitedExecutionContext()

  val cookies = new mutable.HashMap[String, CookieSet]

  override def header(uri: Uri) = Future { cookies.get(uri.getAuthority).map(cs => "Cookie" -> cs.headerValue) }

  override def update(uri: Uri, headers: Headers): Unit = Future {
    val cs = cookies.getOrElseUpdate(uri.getAuthority, new CookieSet())
    Option(headers.getAll("Set-Cookie")).foreach {
      _.asScala foreach {
        case Cookie(c) => cs += c
        case header => warn(s"unexpected cookie header: $header")
      }
    }
  }
}

object CookieStorage {

  implicit val factory = new Factory[CookieStorage](_ => new MemoryCookieStorage)

  @Json
  case class Cookie(key: String, value: String, expires: Date) {
    def expired = expires.before(new Date)
  }

  object Cookie {
    val Pair = """(\w+)=([^;]*)""".r
    val ExpiresFormat = new ThreadLocal[SimpleDateFormat] {
      override def initialValue() = returning(new SimpleDateFormat("EEEE, dd-MMM-yy HH:mm:ss zzz", Locale.US))(_.setTimeZone(TimeZone.getTimeZone("GMT")))
    }

    def unapply(str: String): Option[Cookie] = Try {
      val pairs = Pair.findAllMatchIn(str).map(m => m.group(1) -> m.group(2)).toSeq
      val expires = pairs.find(_._1 == "expires").map(p => ExpiresFormat.get().parse(p._2))
      for {
        (k, v) <- pairs.headOption
        exp <- expires
      } yield Cookie(k, v, exp)
    } .getOrElse(None)
  }

  case class CookieSet(cookies: mutable.HashMap[String, Cookie] = new mutable.HashMap) {

    def +=(c: Cookie) = {
      cookies += c.key -> c
      deleteExpired()
    }

    def deleteExpired() = cookies.find(_._2.expired).toSeq foreach { case (k, _) => cookies.remove(k) }

    def headerValue = cookies.valuesIterator.filterNot(_.expired).map(c => c.key + "=" + c.value).mkString("; ")
  }
}
