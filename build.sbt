ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.0"

lazy val root = (project in file("."))
  .settings(
    name := "lohika",
    libraryDependencies ++= Seq("com.lihaoyi" %% "fastparse" % "3.1.1")
  )
