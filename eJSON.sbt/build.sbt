
val scala3Version: String = "3.4.2"

name := "eJSON"
organization := "com.peterlavalle"


scalaVersion := scala3Version

resolvers += "jitpack" at "https://jitpack.io"
resolvers += "Maven Central" at "https://repo1.maven.org/maven2/"

libraryDependencies += "org.json" % "json" % "20240303"

libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
