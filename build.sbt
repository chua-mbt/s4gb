ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "s4gb",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.4" % Test,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
