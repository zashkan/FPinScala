name := "Functional Programming in Scala - Exercises"

version := "0.0.1"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6" % "test")

libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.6" % "test"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")


