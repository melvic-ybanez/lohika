import Dependencies.*
import sbtassembly.MergeStrategy

ThisBuild / version := "0.5.0"

ThisBuild / scalaVersion := "3.5.0"

lazy val commonMergeStrategy: String => MergeStrategy = {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x                             => MergeStrategy.first
}

lazy val root = (project in file("."))
  .settings(
    name := "lohika",
    assembly / assemblyJarName := "lohika-0.5.0.jar",
    assembly / assemblyMergeStrategy := commonMergeStrategy
  )
  .aggregate(core, ui)

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= fastParse ++ scalaTest ++ cats,
    assembly / assemblyJarName := "lohika-core-0.5.0.jar",
    assembly / assemblyMergeStrategy := commonMergeStrategy
  )

lazy val ui = (project in file("ui"))
  .settings(
    name := "ui",
    libraryDependencies ++= Dependencies.ui,
    assembly / assemblyJarName := "lohika-ui-0.5.0.jar",
    assembly / mainClass := Some("com.melvic.lohika.ui.Main"),
    assembly / assemblyMergeStrategy := commonMergeStrategy
  )
  .dependsOn(core)
