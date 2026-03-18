ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / testFrameworks += new TestFramework("utest.runner.Framework")

val munitVersion = "1.2.4"

lazy val blarggSource = taskKey[Unit]("Ensure blargg test ROMs submodule is initialized")

blarggSource := {
  import sys.process._

  val log = streams.value.log
  val dir = file("blargg-test/src/test/resources/blargg-test-roms")

  if (!dir.exists() || dir.listFiles().isEmpty) {
    log.info("Initializing blargg test ROMs submodule...")
    val exitCode = "git submodule update --init --recursive".!
    if (exitCode != 0) sys.error("Failed to initialize git submodules")
  } else {
    log.info("Blargg test ROMs already present")
  }
}

lazy val gameboy = (project in file("gameboy"))
  .settings(
    name := "s4gb-gameboy",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "spire" % "0.18.0",
      "org.typelevel" %% "spire-macros" % "0.18.0",
      "org.scalameta" %% "munit" % munitVersion % Test
    ),
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
    }
  )

lazy val blarggTest = (project in file("blargg-test"))
  .dependsOn(gameboy)
  .settings(
    name := "s4gb-blargg-test",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % munitVersion % Test
    ),
    Test / resourceDirectory := baseDirectory.value / "src" / "test" / "resources",
    Test / unmanagedResourceDirectories += baseDirectory.value / "src" / "test" / "resources" / "blargg-test-roms"
  )

test := {
  blarggSource.value
  (gameboy / Test / test).value
  (blarggTest / Test / test).value
}