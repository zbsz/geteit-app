package com.geteit.inject

import android.content.{Context, SharedPreferences}
import android.preference.PreferenceManager
import com.geteit.cache.{CacheStorage, CacheService}
import com.geteit.image.{BasicImageProvider, ImageProvider, MemoryImageCache}
import com.geteit.net._

object GtAppModule {

  def apply() = Module { implicit bind =>
    bind [MemoryImageCache] to new MemoryImageCache
    bind [ImageProvider] to new BasicImageProvider
    bind [CacheService] to new CacheService
    bind [CacheStorage] to new CacheStorage
    bind [AsyncClient] to new AsyncClient
    bind [CookieStorage] to new MemoryCookieStorage
    bind [ResponseBodyDecoder] to new DefaultResponseBodyDecoder
    bind [SharedPreferences] to { PreferenceManager.getDefaultSharedPreferences(bind.inject[Context]) }
  }
}
