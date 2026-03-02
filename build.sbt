ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val gameboy = (project in file("gameboy"))
  .settings(
    name := "s4gb-gameboy",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "spire" % "0.18.0",
      "org.typelevel" %% "spire-macros" % "0.18.0",
      "org.scalameta" %% "munit" % "1.2.3" % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings"
    )
  )

lazy val generateOpcodeTable = taskKey[Unit]("Generate HTML opcode table")

lazy val opcodeTable = (project in file("opcode-table"))
  .dependsOn(gameboy)
  .settings(
    name := "s4gb-opcode-table",
    generateOpcodeTable := {
      (Compile / runMain).toTask(" OpcodeTableGenerator").value
    },
    cleanFiles += file("docs")
  )
