## Debugging gaps for simulation correctness

### Problem

When a multi-module design produces wrong output, we have no efficient way
to find which signal has the wrong value at which timestep. The only tool
today is manual bisection with $display.

### Gap 1: VCD waveform dump ($dumpfile / $dumpvars)

Priority: high. This is the standard debugging workflow for every RTL
engineer. Without it, diagnosing riscv-cpu-class bugs requires hours of
manual print insertion.

What it gives us:

- Open trace in GTKWave, see every signal at every timestep
- Immediately compare Lyra vs Verilator traces side-by-side
- Find the exact cycle and signal where behavior diverges

Implementation scope:

- $dumpfile("file.vcd") -- set output file
- $dumpvars(level, scope) -- select signals to record
- Runtime hooks: record signal changes during simulation
- VCD file writer (simple text format)

### Gap 2: Signal change trace mode

A lighter alternative to VCD: text-based output of all signal changes,
like Verilator's --trace but as text. Example:

    @t=25ns: cpu.branch_taken 0 -> 1
    @t=25ns: cpu.pc 20 -> 12

Could be a CLI flag: lyra run --signal-trace

### Current workaround

Manual bisection with $display in initial blocks, plus progressively
simplifying the design until the bug reproduces in a minimal test.
