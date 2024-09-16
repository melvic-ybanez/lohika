import sbt._

object Dependencies {
  lazy val fastParse = Seq("com.lihaoyi" %% "fastparse" % "3.1.1")
  lazy val scalaTest = Seq(
    "org.scalactic" %% "scalactic" % "3.2.19",
    "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % "2.12.0"
  )
  lazy val scalaFx = Seq(
    "org.scalafx" %% "scalafx" % "21.0.0-R32",
    "io.github.mkpaz" % "atlantafx-base" % "2.0.1",
    "org.commonmark" % "commonmark" % "0.21.0"
  )
}