# Gaps

Work queue for codegen, runtime, and toolchain issues. Each entry has a done
definition so it can be closed cleanly. When a gap is resolved, delete the
section entirely.

Tags: `[codegen]` `[runtime]` `[aot]` `[perf]` `[tests]`

## Process duplication across instances [codegen] [perf]

Processes from instantiated modules are duplicated per instance. RISC-V CPU
shows paired processes (e.g., two `always_comb` from `instr_decode.sv:33:3` with
identical instruction counts). For N instances of a module with P processes,
this produces N\*P functions instead of P.

**Fix shape**: share process function code across instances of the same module.
Each instance passes its own state pointer.

**Done**: `--stats` shows one function per unique process source location, not
one per instance.

## Out-of-bounds produces UB [runtime] [codegen]

Array/vector out-of-bounds accesses produce undefined behavior. IEEE 1800
specifies OOB reads return X, OOB writes are ignored.

**Fix shape**: emit bounds checks in LLVM IR before array/vector access. Return
X (or 0 in 2-state mode) for OOB reads, skip store for OOB writes.

**Done**: OOB read returns X (or 0), OOB write is no-op. Test covers both
cases.

## Performance validation [perf] [tests]

No automated tracking of compile time, runtime, or IR size. No way to detect
regressions.

**Fix shape**: benchmark script that runs key designs and records metrics
(compile time, codegen time, peak RSS, IR instruction count, executable size).

**Done**: repeatable metrics report exists. CI runs it on every PR as an
informational step.
