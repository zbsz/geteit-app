package com.geteit.net

import com.geteit.concurrent.CancellableFuture
import com.geteit.util.Log._
import com.koushikdutta.async.http.AsyncHttpClient

/**
 * Wrapper for instrumenting of AsyncHttpClient, by default is empty, but will be replaced in tests.
 */
object ClientWrapper {
  private implicit val logTag: LogTag = "ClientWrapper"

  def apply(client: AsyncHttpClient): CancellableFuture[AsyncHttpClient] = CancellableFuture.successful(client)
}
