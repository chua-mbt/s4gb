ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "s4gb",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "spire" % "0.18.0",
      "org.typelevel" %% "spire-macros" % "0.18.0",
      "org.scalameta" %% "munit" % "1.2.1" % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
