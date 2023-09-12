import sbt.Keys.*
import sbt.*

lazy val root = project
  .in(file(""))
  .settings(
    name := "training",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-Yexplicit-nulls",
      "-source", "future"
    ),
    scalaVersion := "3.4.0-RC1-bin-20230909-64c3138-NIGHTLY"
  )
