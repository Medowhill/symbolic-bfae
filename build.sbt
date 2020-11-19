ThisBuild / scalaVersion := "2.13.2"
ThisBuild / organization := "info.hjaem"
ThisBuild / name := "bfae"

scalacOptions += "-feature"
scalacOptions += "-deprecation"
scalacOptions += "-Xlint:unused"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
