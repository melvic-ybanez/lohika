import Dependencies.*
import sbtassembly.MergeStrategy

ThisBuild / version := Version

ThisBuild / scalaVersion := "3.5.0"

lazy val commonMergeStrategy: String => MergeStrategy = {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x                             => MergeStrategy.first
}

lazy val Version = "0.10.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "lohika",
    assembly / assemblyJarName := s"lohika-$Version.jar",
    assembly / assemblyMergeStrategy := commonMergeStrategy
  )
  .aggregate(core, ui)

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= fastParse ++ scalaTest ++ cats,
    assembly / assemblyJarName := s"lohika-core-$Version.jar",
    assembly / assemblyMergeStrategy := commonMergeStrategy
  )

lazy val ui = (project in file("ui"))
  .settings(
    name := "ui",
    libraryDependencies ++= Dependencies.ui,
    assembly / assemblyJarName := s"lohika-ui-$Version.jar",
    assembly / mainClass := Some("com.melvic.lohika.ui.Main"),
    assembly / assemblyMergeStrategy := commonMergeStrategy
  )
  .dependsOn(controllers)

lazy val controllers = (project in file("controllers"))
  .settings(
    name := "controllers",
    assembly / assemblyJarName := s"lohika-controllers-$Version.jar",
    assembly / assemblyMergeStrategy := commonMergeStrategy
  )
  .dependsOn(core)
