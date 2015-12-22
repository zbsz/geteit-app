package com.geteit.inject

import android.content.{Context, SharedPreferences}
import android.preference.PreferenceManager
import com.geteit.cache.{CacheStorage, CacheService}
import com.geteit.content.KeyValueStorage
import com.geteit.image.{BasicImageProvider, ImageProvider, MemoryImageCache}
import com.geteit.net._

object GtAppModule {

  def apply() = new Module {
    bind [MemoryImageCache] to new MemoryImageCache
    bind [ImageProvider] to new BasicImageProvider
    bind [CacheService] to new CacheService
    bind [CacheStorage] to new CacheStorage
    bind [AsyncClient] to new AsyncClient
    bind [CookieStorage] to new MemoryCookieStorage
    bind [KeyValueStorage] to new KeyValueStorage
    bind [ResponseBodyDecoder] to new DefaultResponseBodyDecoder
    bind [SharedPreferences] to { PreferenceManager.getDefaultSharedPreferences(inject[Context]) }
  }
}
