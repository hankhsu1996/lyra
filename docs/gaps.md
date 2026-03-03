# Gaps

Work queue for codegen, runtime, and toolchain issues. Each entry has a done
definition so it can be closed cleanly. When a gap is resolved, delete the
section entirely.

Gaps tagged `[aot]` are tracked in [aot-readiness.md](aot-readiness.md) with
milestone assignments.

Tags: `[codegen]` `[runtime]` `[aot]` `[perf]` `[tests]` `[examples]`

## RISC-V CPU example broken [examples] [codegen]

Both AOT and JIT produce `x3 = X, expected 55` for sum_test and fib_test. The
register file reads back X instead of the computed value. Same failure on both
backends -- shared codegen or runtime issue, not AOT-specific.

**Fix shape**: Debug by dumping register file writes and reads. Compare against
Verilator trace. May be a sensitivity list issue, an NBA ordering issue, or an
init-time X-taint problem.

**Done**: `lyra -C examples/riscv-cpu run` prints `Results: 2 passed, 0 failed`.

## Performance validation [perf] [tests]

No automated tracking of compile time, runtime, or IR size. No way to detect
regressions. Performance is measured ad-hoc with `-v` and `--stats`.

**Fix shape**:

- Benchmark script that runs key designs and records metrics
- Metrics: compile wall time, codegen wall time, peak RSS, IR instruction
  count, executable size (AOT)
- Start with observation (print report), graduate to CI gate later
- Designs: hello (tiny), RISC-V CPU (medium), synthetic large-array stress test

**Done**: Repeatable metrics report exists. CI runs it on every PR as an
informational step (no fail gate initially).

## JIT lifecycle [codegen] [aot]

Two execution paths (AOT and JIT) with overlapping but divergent code. JIT is
the historical default but AOT is the production direction. Maintaining both
increases surface area.

**Fix shape** (phased):

1. AOT becomes default backend (done, PR #440)
2. JIT moves to `--backend=jit` (done)
3. JIT marked dev-only in docs and README
4. Optional: `LYRA_ENABLE_JIT` build flag (default ON for now)
5. JIT removed when AOT reaches correctness parity

**Done**: JIT documented as dev-only. No CI correctness guarantees beyond basic
compilation.

## Out-of-bounds produces UB [runtime] [codegen]

Array/vector out-of-bounds accesses produce undefined behavior. IEEE 1800
specifies OOB reads return X, OOB writes are ignored. Documented in
[limitations.md](limitations.md).

**Fix shape**: Emit bounds checks in LLVM IR before array/vector access. Return
X (or 0 in 2-state mode) for OOB reads. Skip store for OOB writes. Gate behind
an opt-level flag if performance impact is significant.

**Done**: OOB read returns X (or 0), OOB write is no-op. Test covers both
cases.

## X/Z initialization not observable [codegen]

Variables declared as `logic` should initialize to X per IEEE 1800. LLVM
backend zeroinits all storage. 4-state patch infrastructure exists
(`LyraApply4StatePatches*`) but is not applied to all variables that should
start as X.

**Fix shape**: Ensure all 4-state variables get X-init patches. Verify with
`$display` that uninitialized `logic` shows `x`. Intersects with init IR
explosion (init shape refactor).

**Done**: `logic x; $display("%b", x);` prints `x` in LLVM backend.

## Process duplication across instances [codegen] [perf]

Processes from instantiated modules are duplicated per instance. RISC-V CPU
shows paired processes (e.g., `process_66` and `process_47` both from
`instr_decode.sv:33:3` with identical instruction counts). For `N` instances
of a module with `P` processes, this produces `N*P` functions instead of `P`.

**Fix shape**: Share process function code across instances of the same module.
Each instance passes its own state pointer. Significant codegen architecture
change.

**Done**: `--stats` shows one function per unique process source location, not
one per instance.
