# Blargg CPU Tests

## Running Tests

```bash
sbt "blarggTest/test"
```

## CLI Options

### gameboyDoctorLogs

Enable logging of CPU state for gameboy-doctor comparison.

```bash
sbt "blarggTest/test" -DgameboyDoctorLogs=true
```

Logs will be written to `blargg-test/target/logs/`.

### generateReport

Generate HTML report with test results.

```bash
sbt "blarggTest/test" -DgenerateReport=true
```

Report will be written to `blargg-test/target/reports/report.html`.

## Using Gameboy Doctor

[Gameboy Doctor](https://github.com/robert/gameboy-doctor) compares your emulator's CPU state against a reference implementation to find bugs.

### 1. Generate Logs

Generate logs for the test ROMs:

```bash
sbt "blarggTest/test" -DgameboyDoctorLogs=true
```

### 2. Run Gameboy Doctor

Feed your logs to gameboy-doctor. The test ROM numbers map to:
- 1 = 01-special.gb
- 3 = 03-op sp,hl.gb
- 4 = 04-op r,imm.gb
- 5 = 05-op rp.gb
- 6 = 06-ld r,r.gb
- 7 = 07-jr,jp,call,ret,rst.gb
- 8 = 08-misc instrs.gb
- 9 = 09-op r,r.gb
- 10 = 10-bit ops.gb
- 11 = 11-op a,(hl).gb

Example:
```bash
python3 /path/to/gameboy-doctor blargg-test/target/logs/01-special.log cpu_instrs 1
```

### 3. Success Output

When successful, you'll see:
```
============== SUCCESS ==============

Your logs matched mine for all 1256633 lines - you passed the test ROM!
```

### 4. Error Output

If there's a mismatch, gameboy-doctor will show:
- The line number where divergence occurred
- Your CPU state vs the expected state
- The opcode that caused the divergence
