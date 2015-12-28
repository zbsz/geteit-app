package com.geteit.content

import android.content.Context
import android.database.sqlite.SQLiteOpenHelper
import com.geteit.db.{DaoDB, Migration, Storage}
import com.geteit.events.EventContext
import com.geteit.inject.Module
import com.geteit.json.Json
import com.geteit.model.KeyValue
import com.geteit.model.KeyValue.KeyValueDao
import org.robolectric.RuntimeEnvironment
import org.robolectric.shadows.ShadowLog
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time._
import org.scalatest.{FeatureSpec, Matchers, RobolectricSuite}

class KeyValueStorageSpec extends FeatureSpec with Matchers with RobolectricSuite with ScalaFutures {
  import EventContext.Implicits.global

  override implicit val patienceConfig = PatienceConfig(Span(10, Seconds), Span(100, Millis))

  lazy val context = RuntimeEnvironment.application

  implicit lazy val module = new Module {
    bind[Context]       to context
    bind[EventContext]  to EventContext.Global
    bind[Storage]       to new Storage() {
      override val dbHelper: SQLiteOpenHelper = new SQLiteOpenHelper(context, "key-value-spec", null, 1) with DaoDB {
        override val daos = Seq(KeyValueDao)
        override val migrations: Seq[Migration] = Nil
      }
    }
  }

  feature("Preference signal") {
    lazy val kvStorage = new KeyValueStorage()
    lazy val signal = kvStorage.pref("key1", Item("default"))

    scenario("Get default value from new key") {
      signal.head.future.futureValue shouldEqual Item("default")
    }

    scenario("Set pref value") {
      signal ! Item("changed 1")
      signal.currentValue shouldEqual Some(Item("changed 1"))

      kvStorage.get("key1").futureValue shouldEqual Some(KeyValue("key1", """{"str":"changed 1"}"""))
      kvStorage.storage { KeyValueDao.get("key1")(_) }.futureValue shouldEqual Some(KeyValue("key1", """{"str":"changed 1"}"""))
    }

    scenario("Load new signal with existing key") {
      val signal1 = kvStorage.pref("key1", Item("default1"))
      signal1.head.future.futureValue shouldEqual Item("changed 1")
    }

    scenario("Update newly created signal instance") {
      val signal1 = kvStorage.pref("key1", Item("default2"))
      signal1 ! Item("updated 2")
      signal.filter(_.str == "updated 2").head.future.futureValue shouldEqual Item("updated 2")
    }

    scenario("Load existing key from new KeyValueStorage instance") {
      new KeyValueStorage().pref("key1", Item("default2")).head.future.futureValue shouldEqual Item("updated 2")
    }

    scenario("mutate signal") {
      ShadowLog.stream = System.out
      val signal1 = kvStorage.pref("key1", Item("default2"))
      signal1.head.future.futureValue shouldEqual Item("updated 2")
      signal1.mutate(_ => Item("updated 3"))
      signal.filter(_.str == "updated 3").head.future.futureValue shouldEqual Item("updated 3")

    }
  }
}

@Json
case class Item(str: String)
