package com.geteit.db

import android.database.sqlite.SQLiteDatabase
import com.geteit.util.Log._

import scala.util.control.NonFatal

trait Migration { self =>
  val fromVersion: Int
  val toVersion: Int

  def apply(db: SQLiteDatabase): Unit
}

object Migration {
  val AnyVersion = -1

  def apply(from: Int, to: Int)(migrate: SQLiteDatabase => Unit): Migration = new Migration {
    override val toVersion = to
    override val fromVersion = from

    override def apply(db: SQLiteDatabase): Unit = migrate(db)
  }

  def to(to: Int)(migrate: SQLiteDatabase => Unit): Migration = apply(AnyVersion, to)(migrate)
}

/**
 * Uses given list of migrations to migrate database from one version to another.
 * Finds shortest migration path and applies it.
 */
class Migrations(migrations: Migration*) {

  private implicit val logTag: LogTag = logTagFor[Migrations]
  val toVersionMap = migrations.groupBy(_.toVersion)

  def plan(from: Int, to: Int): List[Migration] = {

    def shortest(from: Int, to: Int): List[Migration] = {
      val possible = toVersionMap.getOrElse(to, Nil)
      val plans = possible.map { m =>
        if (m.fromVersion == from || m.fromVersion == Migration.AnyVersion) List(m)
        else if (m.fromVersion < from) Nil
        else shortest(from, m.fromVersion) match {
          case Nil => List()
          case best => best ::: List(m)
        }
      } .filter(_.nonEmpty)

      if (plans.isEmpty) Nil
      else plans.minBy(_.length)
    }

    if (from == to) Nil
    else shortest(from, to)
  }

  /**
   * Migrates database using provided migrations.
   * Falls back to dropping all data if migration fails.
   * @throws IllegalStateException if no migration plan can be found
   */
  @throws[IllegalStateException]("If no migration plan can be found for given versions")
  def migrate(storage: DaoDB, fromVersion: Int, toVersion: Int)(implicit db: SQLiteDatabase): Unit = {
    if (fromVersion != toVersion) {
      plan(fromVersion, toVersion) match {
        case Nil => throw new IllegalStateException(s"No migration plan from: $fromVersion to: $toVersion")
        case ms =>
          try {
            db.beginTransaction()
            ms.foreach(_.apply(db))
            db.setTransactionSuccessful()
          } catch {
            case NonFatal(e) =>
              error(s"Migration failed for $storage, from: $fromVersion to: $toVersion", e)
              fallback(storage, db)
              db.setTransactionSuccessful()
          } finally {
            db.endTransaction()
          }
      }
    }
  }

  def fallback(storage: DaoDB, db: SQLiteDatabase): Unit = {
    warn(s"Dropping all data for $storage.")
    storage.dropAllTables(db)
    storage.onCreate(db)
  }
}
