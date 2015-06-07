package com.geteit.bitmap

import android.support.v8.renderscript.{RSRuntimeException, RenderScript}
import com.geteit.app.GtApplication
import com.geteit.concurrent.{CancellableFuture, LimitedExecutionContext}
import com.geteit.util.Log._

import scala.util.control.NoStackTrace

object RenderScriptContext {
  implicit val dispatcher = new LimitedExecutionContext()
  private implicit val logTag: LogTag = "RenderScriptContext"

  case object RSNotAvailableException extends Exception("RenderScript not available") with NoStackTrace

  lazy val rs = try {
    RenderScript.create(GtApplication.APP_INSTANCE)
  } catch {
    case e: RSRuntimeException =>
      warn(s"Renderscript context could not be instantiated.", e)
      null
  }

  def apply[A](body: RenderScript => A) = CancellableFuture { rs } flatMap {
    case null => CancellableFuture.failed(RSNotAvailableException)
    case r => CancellableFuture.successful(body(r))
  }
}
