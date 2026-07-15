# Architecture Reset

Tracks the work to bring the post-reset architecture to completeness. The reset rebuilt the compiler
around per-unit compilation and a runtime-elaborated object graph (see
`../architecture/north_star.md`). This file is the top-level index of what that rebuild still owes,
in four parts:

- **Infrastructure** -- pipeline and build surfaces the reset has not finished rebuilding.
- **SystemVerilog features** -- language surfaces to reproduce. The archive under
  `archived/tests/sv_features/<path>` documents the expected behavior; where a workstream has its
  own progress file, that file owns the sub-steps and the item here is the roll-up.
- **Scheduling and sensitivity** -- the runtime engine's region semantics and change detection.
- **Retired concepts** -- archive surfaces the reset dissolved, recorded with the decision that
  dissolved each so they are not reintroduced.

An item is checked only when its behavior is reproduced on the current architecture, or, for an
infrastructure surface, built.

## Infrastructure

- [x] LIR -- the execution-oriented IR (CFG, basic blocks, storage). Contract in
      `../architecture/lir.md`.
- [ ] Execution backend -- MIR / LIR lowered to LLVM IR, consumed as either an AOT binary or a JIT
      image. JIT and AOT are link-time choices over one backend, not separate surfaces. Contracts in
      `../architecture/backend_contract.md` and `../architecture/runtime_distribution.md`. Design
      elaboration runs on both backends as the synthesized design-root unit's construct
      (`../decisions/root-unit-elaboration.md`): the execution backend lowers cross-unit
      construction and realizes members as runtime-owned storage
      (`../decisions/member-slot-storage.md`), so it elaborates a hierarchy of modules through the
      design-root, matching the C++ backend. It also runs procedural code over the integral and
      string value domains -- variables, expressions, structured control flow, and value-carrying
      formatted output -- with control flow lowered to a control-flow graph. It calls foreign C: a
      DPI-C import lowers to an external-linkage symbol, marshals the by-value carriers, and
      resolves the symbol through the execution session (`dpi.md`). It suspends a process on a
      delay, on a value change (`@(...)`, `@*`, `always_comb`, a continuous assignment), and on a
      level wait. Still open on the execution backend: the remaining value domains (reals,
      enumerations, aggregates, containers), a value whose lifetime crosses a suspension (so a
      loop-carried value around a wait, which a clock generator needs), named events, non-blocking
      assignment, subroutine reference arguments, closures, by-pointer DPI-C marshaling, and native
      layout for value members (the baseline keeps runtime-owned cells).
- [ ] Per-unit artifact emission -- one artifact per unit specialization, assembled by linking,
      replacing the transitional single-`main.cpp` aggregation. Contract in
      `../architecture/emission_model.md` (inv 1).
- [ ] Incremental and parallel compilation -- query-based recompilation of only the units a change
      touches (`north_star` inv 3). Contract in `../architecture/incremental_build.md`.
- [ ] Object lifetime and reclamation -- managed object lifetime and the class / module object-model
      unification. Tracked in `object-model.md`; contract in `../architecture/object_lifetime.md`.
- [ ] CLI execution surface -- `dump llvm` (rides on LIR), and `run` / `compile` end-to-end against
      the execution backend.
- [ ] Execution CI jobs -- smoke, benchmark, and AOT jobs (currently disabled; see
      `../ci/README.md`), rebuilt once the execution backend lands.
- [x] Test-framework variable assertions (`expect.variables`) -- `tests/framework/runner.cpp`.

## SystemVerilog features

### assertions

- [ ] assertions/default -- immediate assertions (LRM 16.4). Currently rejected at lowering
      (`--disable-assertions`).
- [ ] assertions/deferred -- deferred assertion actions (`assert #0`, LRM 16.4), with encounter-time
      vs execution-time capture semantics.
- [ ] assertions/cover -- cover statements (LRM 16.7). Cover-hit observability is a tool feature,
      not a language one (the archive `jit_only` framing assumed an obsolete JIT-vs-AOT split).

### compilation_units

- [ ] compilation_units/default -- single-unit vs per-file macro visibility (LRM 3.12.1). The
      `--single-unit` CLI flag and the model (`../architecture/compilation_unit_model.md`) exist;
      archive coverage to verify.

### control_flow

- [x] control_flow -- `control-flow.md` (case, conditional, loops, foreach, ternary, unique /
      priority).

### datatypes

Granular tracking lives in `integral.md` and `datatypes.md`.

- [x] datatypes/default_init -- LRM Table 6-7 default initialization across families and nesting.
- [x] datatypes/enum
- [x] datatypes/general -- parameters, typedef, and chandle.
- [x] datatypes/integral -- `integral.md`.
- [x] datatypes/packed -- `packed.md`.
- [x] datatypes/real
- [x] datatypes/representation -- X/Z representation, including a wide value carrying X/Z across the
      64-bit word boundary.
- [x] datatypes/string -- `datatypes.md`.
- [x] datatypes/unpacked -- fixed unpacked arrays, the variable-size aggregate family
      (`aggregate.md`), and unpacked struct / union.
- [x] datatypes/wide_integral -- `integral.md`; wide (>64-bit) packed 2D element index / slice.

### directives

- [ ] directives/timescale -- `timescale.md`.
- [ ] directives/time_literals -- time literals and unit scaling (`timescale.md`).

### dpi

Tracked in `dpi.md` (the full LRM 35 import / export surface).

- [ ] DPI-C -- import, export, DPI tasks, context and the `svdpi` surface, open arrays, and link
      orchestration (LRM 35). Import runs on both backends -- the full type surface on the C++
      backend, the by-value scalar surface on the execution backend. Export, tasks, context, and
      open arrays remain.

### fork_join

- [ ] fork / join / join_any / join_none -- `fork-join.md`.

### functions

Tracked in `functions.md` (the full LRM 13 subroutine surface).

- [x] functions/basic -- F1, F2, F5.
- [x] functions/nested_calls -- F1.
- [x] functions/recursion -- F3.
- [x] functions/string_return -- F7.
- [x] functions/container_params -- F7, F8.
- [x] functions/container_outparams -- F4, F8.
- [x] functions/chandle -- F9.

### generate

- [ ] generate/generate -- if / case / for-generate, nesting, genvars, processes and instances
      inside generate, and hierarchical references through generate all lower and run; so does a
      genvar-conditional branch (an `if` reading the genvar inside a `for`-generate). The archive
      suite is not yet migrated.

### hierarchy

Tracked in `hierarchy.md`.

- [x] hierarchy/instantiation -- multi-unit compilation, parameter specialization, the object tree,
      nested hierarchy, instance arrays, generate-wrapped instances (Stages A, B).
- [x] hierarchy/ports -- directions, named / positional / expression / constant connections,
      defaults, `ref` ports, pass-through, instance-array and non-integral ports (Stage E).
      Net-typed ports follow the net model.
- [x] hierarchy/refs -- downward and upward hierarchical references at any depth, through generate
      scopes and instance arrays, with cross-instance sensitivity and `%m` paths (Stages C, D),
      including a reference whose head is a named procedural block or a named fork-join block.

### nets

- [ ] Nets and net resolution -- a net's value as the resolution of its driver contributions,
      net-typed ports, and multi-driver resolution (`nets.md`).

### operators

Tracked in `operators.md` and `integral.md`.

- [x] operators/binary -- `integral.md`.
- [x] operators/binary_string -- `datatypes.md` SC1.
- [x] operators/case_equality -- `operators.md`.
- [x] operators/wildcard_equality -- `operators.md`.
- [x] operators/compound_assignment -- `operators.md`.
- [x] operators/concat -- `operators.md`.
- [x] operators/inside -- `operators.md`.
- [x] operators/replicate -- `operators.md`.
- [x] operators/replication_patterns -- `packed.md`.
- [x] operators/shift_overflow -- `integral.md`.
- [x] operators/unary -- `integral.md`; `++` / `--` via `operators.md`.
- [x] operators/comparison_value_temp -- comparison results used as operands (`integral.md`). The
      named `ValueTemp` IR is retired; the behavior is ordinary expression handling.
- [x] operators/value_temp_expansion -- a select or index on a temporary expression result (concat,
      replication, computed array index, function return). Same retired-IR note as above.

### packages

- [ ] packages/basic -- package-level declarations and cross-package import (LRM 26). No package
      lowering exists.
- [ ] packages/functions -- functions exported from packages (LRM 26.3); rides on packages/basic.

### processes

Tracked in `processes.md`.

- [x] processes/initial -- P1.
- [x] processes/final_block -- P2.
- [x] processes/continuous_assign -- P7.
- [x] processes/non_blocking -- P4.
- [x] processes/delay -- T1.
- [x] processes/event_triggers -- T2..T4.
- [x] processes/named_event -- P9.
- [x] processes/wait_event -- P11.
- [ ] processes/edge_trigger_bit_select -- packed bit / range / part-select edge sources, including
      multi-dimensional and ascending / negative-base ranges, are covered (T2..T5); struct-field and
      unpacked-element edge sources remain.
- [ ] processes/edge_trigger_dynamic -- compound event expressions (concatenation, arithmetic,
      dynamic index); needs the snapshot + re-eval wrapper.
- [ ] processes/generate -- process generate; if / for-generate with `initial` and continuous-assign
      bodies run today, the full generate-form x procedural-block matrix is unverified (P12).

### system functions

- [ ] Query functions -- the data-query and array-query system functions (`$bits`, `$typename`,
      `$left` / `$right` / `$low` / `$high` / `$increment` / `$size`, `$dimensions` /
      `$unpacked_dimensions`), as elaboration-time constants for fixed-size operands and runtime
      queries for dynamically sized ones (`query-functions.md`). The value-computation functions
      (`$clog2`, `$signed` / `$unsigned`, `$isunknown` / `$countones` / `$onehot`) are supported.

### system tasks

- [ ] System tasks -- the `$display` / `$write` / `$strobe` / `$monitor` family, file IO,
      `$sformat`, `$time` / `$timeformat`, `$random`, `$readmem*`. Formatting is tracked in
      `display.md` and RNG in `simulation-rng.md`; most land in the runtime call registry. Known
      gaps: `$monitor`, `$random`, `$readmem*`. (The archive effect / pure / state classification is
      retired -- see below.)

### trace

- [ ] trace/signal_trace -- text signal-change trace output (`--trace-signals`), layered on the
      `Var<T>` subscription model. (The trace / update-set subsystem is retired -- see below.)

## Scheduling and sensitivity

The stratified region scheduler (`../architecture/scheduling.md`) is implemented in the runtime
engine: LRM 4.4 region order, NBA commit, and `#0` Inactive-region semantics. The core is exercised
by `processes.md` (non-blocking, delay, events). What remains is mapping the archive scheduling
suite onto the current tests, and narrowing change detection below whole-variable granularity.

- [ ] scheduling/regions, nba_ordering, nba_packed, nba_write_modes, zero_delay, zero_delay_time --
      LRM 4.4 region and NBA semantics. The engine implements them; the archive scheduling suite is
      not yet reproduced as current tests.
- [ ] Sensitivity narrowing -- bit-, element-, and field-level any-change so a process wakes only on
      the sub-part it reads (archive `scheduling/sub_slot_observation`, `managed_aggregate_notify`).
      Reads currently collapse to whole-variable subscription (`processes.md` P10 / P13).

## Retired concepts

These archive surfaces named structures of the pre-reset engine and IR that the current architecture
does not have. They are recorded so they are not reintroduced as work; do not reproduce them.

| Archive surface                           | Why it no longer exists                                                                                                                                 | Dissolved by                                                |
| ----------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------- |
| `mutation/*`                              | A write goes through the `Var<T>` store boundary; there is no separate mutation-op IR family.                                                           | `../decisions/value-store-discipline.md`                    |
| `trace/{basic, bulk_ops, update_set}`     | `Var<T>::Set` notifies subscribers at write time; there is no trace / update-set subsystem. (User-facing `signal_trace` survives as a feature.)         | `../decisions/value-store-discipline.md`                    |
| `metadata/slot_meta`                      | Type metadata lives in the MIR type system; there is no runtime slot-metadata registry.                                                                 | `../architecture/mir.md`, `../architecture/object_model.md` |
| `system_tf/{effect, pure, state}`         | The effect / pure / state classification of system tasks dissolved; every runtime effect is a generic `CallExpr`. (The tasks themselves: above.)        | `../decisions/runtime-effects-as-generic-calls.md`          |
| `scheduling/edge_refresh_reconcile`       | Old-engine internals (DeltaDirtySlots, post-flush refresh); not in the stratified-scheduler contract.                                                   | `../architecture/scheduling.md`                             |
| `scheduling/nba_routing`                  | Old-engine per-signal queue routing (deferred-local vs generic queue); the new engine submits NBA closures to one region.                               | `../architecture/scheduling.md`                             |
| `optimization/*`                          | Part-select write correctness is intrinsic to the slice / store model; backend performance work belongs to the execution backend.                       | `../decisions/slice-value-semantics.md`, `performance.md`   |
| `lifecycle/{copy_assign, move_aggregate}` | The lifecycle IR family is gone; value copy / move independence is intrinsic to the value model and covered by the struct, aggregate, and string tests. | `../decisions/value-store-discipline.md`                    |
