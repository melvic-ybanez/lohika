ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.0"

val fastParse = Seq("com.lihaoyi" %% "fastparse" % "3.1.1")
val scalaTest = Seq(
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test"
)
val cats = Seq(
  "org.typelevel" %% "cats-core" % "2.12.0"
)
val scalaFx = Seq(
  "org.scalafx" %% "scalafx" % "21.0.0-R32",
  "io.github.mkpaz" % "atlantafx-base" % "2.0.1"
)

lazy val root = (project in file("."))
  .settings(name := "lohika")
  .aggregate(core, ui)

lazy val core = (project in file("core"))
  .settings(name := "core", libraryDependencies ++= fastParse ++ scalaTest ++ cats)

lazy val ui = (project in file("ui"))
  .settings(name := "ui", libraryDependencies ++= scalaFx)
  .dependsOn(core)