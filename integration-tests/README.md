# Integration Tests

Runs Game Boy test ROMs against the emulator and reports pass/fail per ROM.

## Commands

Run the whole build's tests, including these:

```bash
sbt test
```

Run only this subproject:

```bash
sbt "integrationTests/test"
```

## Options

### generateReport

Write an HTML report of the results. Off by default.

```bash
sbt "integrationTests/test" -DgenerateReport=true
```

Report is written to `integration-tests/target/reports/report.html`.

## ROMs

| Suite | Source | Command |
|-------|--------|---------|
| blargg | git submodule at `src/test/resources/blargg-test-roms` | `sbt "integrationTests/romSources"` |
| Mooneye | pinned archive, cached under `.rom-cache/` | `sbt "integrationTests/mooneyeRoms"` |
| Mealybug Tearoom | pinned archive, cached under `.rom-cache/` | `sbt "integrationTests/mealybugRoms"` |

The two fetched suites are pinned to an exact build, so builds are reproducible. They
cache under `integration-tests/.rom-cache/`, which is gitignored, so a `clean` does not
refetch. Neither is wired into a test run yet.
