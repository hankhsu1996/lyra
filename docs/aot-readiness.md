# AOT Readiness

Milestones and gates for making AOT the production execution path. Each
milestone has a quantitative gate that can eventually become a CI check.

## Milestones

### M0: AOT builds and runs core examples

AOT pipeline exists and can compile+execute simple designs.

**Gate**: `lyra compile -o out examples/hello/*.sv` produces a working
executable. `lyra run` defaults to AOT.

**Status**: Done (PR #440).

### M1: AOT correctness parity

AOT passes the same test corpus as JIT. Examples work end-to-end.

**Gate**: `aot_dev_tests` CI job passes. RISC-V CPU example passes.

**Gaps**:

- RISC-V CPU example broken (P0)
- AOT test coverage (P0)
- X/Z initialization not observable (P1)

**PR sequence**:

1. Fix RISC-V example -- unblocks regression baseline
2. Add `aot_dev_tests` suite -- locks correctness in CI
3. X/Z init observability -- correctness for 4-state designs

### M2: AOT performance baseline

Compile time and binary size are tracked. Regressions are caught.

**Gate**: Benchmark script runs in CI. Init IR explosion is resolved for
medium-size designs (RISC-V CPU level). Init codegen is done (`main` 2579 ->
165 insts on RISC-V CPU).

**Gaps**:

- Performance validation (P1)
- Specialization-based compilation (P2)

**PR sequence**:

1. Add IR size report to CI (observation only) -- establishes baseline
2. Specialization-based compilation -- compile per specialization, not per instance

### M3: JIT dev-only

JIT is no longer a supported production path. AOT is the only user-facing
backend.

**Gate**: JIT is documented as dev-only. JIT test suite is optional in CI (does
not block merge).

**Gaps**:

- JIT lifecycle (P2)

**Prerequisite**: M1 and M2 are complete.

## PR ordering

Priority is top-to-bottom. Each PR should be independently mergeable.

1. Fix RISC-V CPU example
2. Add `aot_dev_tests` suite
3. Add IR size / compile-time report (observation mode)
4. X/Z init observability
5. Specialization-based compilation
6. JIT dev-only documentation and CI changes
