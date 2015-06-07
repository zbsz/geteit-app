import Keys._

import android.Keys._

android.Plugin.androidBuildAar

name := "geteit-app"
organization := "com.geteit"
version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

scalacOptions in Compile ++= Seq("-feature", "-language:implicitConversions", "-language:postfixOps", "-target:jvm-1.6")
platformTarget in Android := "android-21"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
resolvers += "geteit releases" at "https://raw.github.com/zbsz/mvn-repo/master/releases/"

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
  "com.geteit" %% "geteit-utils" % "0.3-SNAPSHOT",
  "com.koushikdutta.async" % "androidasync" % "2.1.3",
  "org.scalatest" %% "scalatest" % "2.2.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.11.6" % Test,
  "com.geteit" %% "robotest" % "0.8" % Test
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

