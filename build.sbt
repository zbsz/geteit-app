import Keys._

import android.Keys._

android.Plugin.androidBuildAar

name := "geteit-app"
organization := "com.geteit"
version := "0.2-SNAPSHOT"

scalaVersion := "2.11.7"

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")
scalacOptions in Compile ++= Seq("-feature", "-language:implicitConversions", "-language:postfixOps", "-target:jvm-1.7")
platformTarget in Android := "android-21"

resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

fork in Test := true
publishArtifact in (Compile, packageDoc) := false
publishArtifact in Test := false
publishArtifact in Compile := false

libraryProject in Android := true

transitiveAndroidLibs in Android := true

val supportLibVersion = "22.1.1"

libraryDependencies ++= Seq (
  "com.android.support" % "support-v4" % supportLibVersion,
  "com.android.support" % "appcompat-v7" % supportLibVersion,
  "com.android.support" % "cardview-v7" % supportLibVersion,
  "com.android.support" % "recyclerview-v7" % supportLibVersion,
  "com.nineoldandroids" % "library" % "2.4.0",
  "com.geteit" %% "geteit-utils" % "0.4-SNAPSHOT",
  "com.koushikdutta.async" % "androidasync" % "2.1.3",
  "org.scalatest" %% "scalatest" % "2.2.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.11.6" % Test,
  "com.geteit" %% "robotest" % "0.+" % Test
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
