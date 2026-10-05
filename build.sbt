ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / testFrameworks += new TestFramework("utest.runner.Framework")

val munitVersion = "1.2.4"

lazy val romSources = taskKey[Unit]("Ensure the blargg test ROM submodule is initialized")
lazy val mealybugRoms = taskKey[File]("Fetch and unpack the prebuilt Mealybug Tearoom ROMs")
lazy val mooneyeRoms = taskKey[File]("Fetch and unpack the prebuilt Mooneye test suite ROMs")

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

lazy val integrationTests = (project in file("integration-tests"))
  .dependsOn(gameboy)
  .settings(
    name := "s4gb-integration-tests",
    romSources := {
      import sys.process._

      val log = streams.value.log
      val dir = baseDirectory.value / "src" / "test" / "resources" / "blargg-test-roms"

      if (!dir.exists() || dir.listFiles().isEmpty) {
        log.info("Initializing blargg test ROMs submodule...")
        val exitCode = "git submodule update --init --recursive".!
        if (exitCode != 0) sys.error("Failed to initialize git submodules")
      } else {
        log.info("Blargg test ROMs already present")
      }
    },
    /** Mealybug Tearoom: 31 prebuilt ROMs from a 45KB zip, pinned to a commit SHA. */
    mealybugRoms := RomSources.fetch(
      streams.value.log,
      baseDirectory.value,
      "mealybug-tearoom-tests",
      "https://raw.githubusercontent.com/mattcurrie/mealybug-tearoom-tests" +
        "/70e88fb90b59d19dfbb9c3ac36c64105202bb1f4/mealybug-tearoom-tests.zip"
    ),
    /** Mooneye: 115 prebuilt ROMs from a pinned per-build archive, PPU tests under acceptance/ppu. */
    mooneyeRoms := RomSources.fetch(
      streams.value.log,
      baseDirectory.value,
      "mooneye-test-suite",
      "https://gekkio.fi/files/mooneye-test-suite/mts-20260714-0944-31510e1" +
        "/mts-20260714-0944-31510e1.zip"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % munitVersion % Test
    ),
    Test / fork := true,
    Test / logBuffered := false,
    Test / javaOptions ++= Seq(
      s"-Ds4gb.roms.blargg=${(baseDirectory.value / "src" / "test" / "resources" / "blargg-test-roms").getPath}",
      s"-Ds4gb.roms.mooneye=${(baseDirectory.value / ".rom-cache" / "mooneye-test-suite").getPath}",
    ),
    // Tests are forked, so a -D on the sbt command line stays in the sbt JVM.
    // Forward it explicitly or it silently does nothing.
    Test / javaOptions ++= sys.props.get("generateReport").map(v => s"-DgenerateReport=$v").toSeq,
    Test / javaOptions ++= sys.props.get("s4gb.mooneye.roms").map(v => s"-Ds4gb.mooneye.roms=$v").toSeq,
    // Forked tests run with the subproject directory as their cwd, so the report
    // directory is passed in absolute rather than resolved against the repo root.
    Test / javaOptions += s"-Ds4gb.reports=${(baseDirectory.value / "target" / "reports").getPath}"
  )

// Prerequisite ROM acquisition for the integration suites. Both tasks are
    // idempotent no-ops once their content is on disk, so they are cheap to leave
    // here. `gameboy/test` depends on neither.
    test := {
      (integrationTests / romSources).value
      (integrationTests / mooneyeRoms).value
      (gameboy / Test / test).value
      (integrationTests / Test / test).value
    }