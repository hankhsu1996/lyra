# Gaps

Work queue for codegen, runtime, and toolchain issues. Each entry has a done
definition so it can be closed cleanly. When a gap is resolved, delete the
section entirely.

Gaps tagged `[aot]` are tracked in [aot-readiness.md](aot-readiness.md) with
milestone assignments.

Tags: `[codegen]` `[runtime]` `[aot]` `[perf]` `[tests]` `[examples]`

## Init IR explosion [codegen] [perf] [aot]

`main` function dominates instruction count. On RISC-V CPU example (87
functions, 12.9K insts), `main` is 2579 insts (20%). On Ibex-scale designs
(~1351 functions, ~290K insts), the ratio is worse and JIT codegen takes 55+
seconds.

Design-state initialization is fully unrolled -- every field gets an individual
store. Large arrays and structs produce linear sequences of `store` instructions
instead of `memset`/`memcpy`/loop patterns.

**Fix shape**:

- Zero-init with `memset` (already zeroinit'd, most fields are zero)
- 4-state X patches via `LyraApply4StatePatches*` (already exists for unknowns)
- Non-zero constants via `memcpy` from a global constant blob
- Large repeated patterns via loop codegen
- Factor `main` into `top_init()` + per-module `init_<N>()` functions

**Done**: `main` instruction count drops below 500 on RISC-V CPU example.
`--stats` shows init functions outside the top 5 for designs with >50
processes.

## AOT test coverage [tests] [aot]

No CI job validates AOT. The `jit_dev_tests` and `mir_dev_tests` suites exist
but there is no `aot_dev_tests` suite.

**Fix shape**:

- Add `aot_dev` suite to `tests/suites.yaml` (same test files as `jit_dev`)
- Add `//tests:aot_dev_tests` target to `tests/BUILD.bazel`
- Add AOT test step to `.github/workflows/bazel-build.yml`

**Done**: `bazel test //tests:aot_dev_tests` runs in CI and passes.

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
