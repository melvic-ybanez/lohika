ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.0"

val fastParse = Seq("com.lihaoyi" %% "fastparse" % "3.1.1")
val scalaTest = Seq(
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "lohika",
    libraryDependencies ++= fastParse ++ scalaTest
  )
