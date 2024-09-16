import Dependencies._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.0"

lazy val root = (project in file("."))
  .settings(name := "lohika")
  .aggregate(core, ui)

lazy val core = (project in file("core"))
  .settings(name := "core", libraryDependencies ++= fastParse ++ scalaTest ++ cats)

lazy val ui = (project in file("ui"))
  .settings(name := "ui", libraryDependencies ++= scalaFx)
  .dependsOn(core)