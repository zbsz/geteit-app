package com.geteit.net

import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone}

import android.net.Uri
import com.geteit.concurrent.{Threading, LimitedExecutionContext}
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
  def header(uri: Uri): Future[Option[(String, String)]] = getCookies(uri.getAuthority) .map { cs => if (cs.isEmpty) None else Some("Cookie" -> Cookie.headerValue(cs)) } (Threading.global)
  def update(uri: Uri, headers: Headers): Unit = {
    Option(headers.getAll("Set-Cookie")).foreach { headers =>
      setCookies(uri.getAuthority, headers.asScala.collect { case Cookie(c) => c })
    }
  }

  def getCookies(authority: String): Future[Seq[Cookie]]
  def setCookies(authority: String, cs: Seq[Cookie]): Unit
}

class MemoryCookieStorage extends CookieStorage {
  private implicit val tag: LogTag = "CookieStorage"
  private implicit val dispatcher = new LimitedExecutionContext()

  val cookies = new mutable.HashMap[String, CookieSet]

  override def getCookies(authority: String): Future[Seq[Cookie]] = Future { cookies.get(authority).map(_.cookies.values.toSeq).getOrElse(Nil) }
  override def setCookies(authority: String, cs: Seq[Cookie]): Unit = Future {
    val set = cookies.getOrElseUpdate(authority, new CookieSet())
    cs foreach { set += _ }
    set.deleteExpired()
  }
}

object CookieStorage {

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

    def headerValue(cs: Traversable[Cookie]) = cs.filterNot(_.expired).map(c => c.key + "=" + c.value).mkString("; ")
  }

  case class CookieSet(cookies: mutable.HashMap[String, Cookie] = new mutable.HashMap) {

    def +=(c: Cookie) = {
      cookies += c.key -> c
      deleteExpired()
    }

    def deleteExpired() = cookies.find(_._2.expired).toSeq foreach { case (k, _) => cookies.remove(k) }

    def headerValue = Cookie.headerValue(cookies.values)
  }

  object CookieSet {
    def apply(cs: Traversable[(String, Cookie)]): CookieSet = new CookieSet(new mutable.HashMap[String, Cookie] ++= cs)
  }
}
