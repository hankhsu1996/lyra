# Processes

Tracks SystemVerilog procedural-block constructs and the supporting timing-control machinery. Covers
archive items under `archived/tests/sv_features/processes/`. Each archive item checkbox in
`architecture-reset.md` is checked when its `*.yaml` cases reproduce on the current pipeline.

The numeric IDs (P1..P11) are stable references to archive items and do **not** imply execution
order. Items in the [Actionable](#actionable) section can be picked up next; items in
[Blocked](#blocked) wait on machinery owned by another workstream.

## Actionable

| Item | Notes                                                                  |
| ---- | ---------------------------------------------------------------------- |
| -    | No procedural-block items left in this workstream's actionable column. |

## Blocked

| Item   | Blocked on                                                                                                            |
| ------ | --------------------------------------------------------------------------------------------------------------------- |
| P5, P6 | Bit-select / range-select expressions in MIR (`operators.md` W4 / W5). Edge triggers on selected lvalues need them.   |
| P8     | `fork` / `join_*`. Needs concurrent-process scheduling primitives; out of scope until single-process work is settled. |

## Sub-Steps

### Procedural blocks

- [x] P1 -- `initial`. `hir::ProcessKind::kInitial` lowers straight-line into a `mir::Process` of
      kind `kInitial`; runtime registers it in the active queue at time 0. The coroutine completes
      naturally when the body finishes. Coverage rides on every other test in `tests/cases/cpp/`.
- [x] P2 -- `final`. `hir::ProcessKind::kFinal` lowers straight-line into a `mir::Process` of kind
      `kFinal`; runtime drains the finals queue at end of simulation. `$finish` inside a `final`
      ends simulation immediately (LRM 9.2.3). Time-controlling statements inside `final` are
      rejected at the runtime boundary because they would suspend a coroutine the engine cannot
      resume.
- [x] P3 -- `always` / `always_ff`. HIR keeps the four SV kinds (`kAlways`, `kAlwaysComb`,
      `kAlwaysLatch`, `kAlwaysFf`). HIR -> MIR wraps the body in
      `mir::ForStmt(empty, empty, empty,     body)` and emits `ProcessKind::kInitial`. `always_ff`
      collapses to the same shape as `always` because the LRM 9.2.2.4 restrictions (one event
      control, no blocking timing) are lint-only and the frontend already enforces them.
      Pathological zero-delay loops (`always areg     = ~areg;`) are caught by the engine's
      `kMaxCurrentTimeIterations` settle limit. Covers `processes/initial` (always portion) and the
      `always_ff` body of `processes/wait_event`.
- [x] P10 / P13 -- `always_comb` / `always_latch` (LRM 9.2.2.2.1) and `always @*` / `always @(*)`
      (LRM 9.4.2.2). Slang's `AnalysisManager` produces read sets (per-procedure for always_comb /
      always_latch, per-region for `@*`); the lyra pipeline stores them in `SensitivityReadStore`
      keyed by `slang::ast::Statement*` (see `docs/decisions/read-set-inference.md` for the shared
      inference architecture). HIR carries them on `Process::implicit_sensitivity_list` for the
      always_comb / always_latch forms and on `ImplicitEventControl::sensitivity_list` for `@*`. HIR
      -> MIR: always_comb / always_latch run `forever { body; SensitivityWaitStmt; }` (body at t=0
      then wait per LRM 9.2.2.2); `@*` expands to `Block { SensitivityWaitStmt; body; }` (wait first
      per LRM 9.4.2.2.2). Reads collapse to whole-variable subscription until the runtime supports
      bit-level any-change.

### Procedural assignments

- [x] P4 -- Non-blocking assignment `<=`. Submits a closure to the NBA region during the kActive
      phase; the engine commits all closures in the kCommitNba phase before observing changes.
      Re-entrancy (`SubmitNba` during NBA commit) is rejected. Covers the basic NBA cases used by
      every `always_ff` example.
- [x] P7 -- Continuous assignment `assign x = y;` (LRM 10.3). Source-aligned in HIR as a scope-level
      `ContinuousAssign {lhs, rhs, sensitivity_list}` whose LHS / RHS land in the enclosing
      structural scope's expr pool alongside parameters and variable initialisers. Read set is
      precomputed by slang's flow analysis (slang treats continuous assignment as a procedure for
      analysis purposes, LRM 10.3.2 reads identical to LRM 9.2.2.2.1); the AST -> HIR lowering looks
      it up keyed by the assignment expression (see `docs/decisions/read-set-inference.md`). HIR ->
      MIR materialises a `mir::Process { kInitial, forever { lhs = rhs; SensitivityWait(reads); } }`
      directly. Drive strength (10.3.4) and `assign #N` (10.3.3) are diagnosed; concat LHS rides on
      the procedural side. Closes `processes/continuous_assign`.

### Timing controls

- [x] T1 -- Delay control `#N`. `mir::TimedStmt` with `DelayControl{duration}`; the engine schedules
      the suspended coroutine onto the delayed queue. Covers `processes/delay`.
- [x] T2 -- Event control `@(x)` any-change. `mir::EventControl{triggers: [{signal, kAnyChange}]}`.
      `Var<T>` wrapper compares old vs new values and notifies the engine on any inequality. Covers
      `processes/event_triggers` (any-change subset).
- [x] T3 -- Event control edge polarity `@(posedge clk)` / `@(negedge clk)` / `@(edge clk)`.
      `EventEdge::{kPosedge,     kNegedge, kBothEdges, kAnyChange}` on each trigger;
      `Observable::TakeMatchingWaiters` filters by edge per LRM Table 9-2. `ClassifyEdge` in
      `runtime/var.hpp` covers the full 4-state transition table: posedge fires on 0 -> {1, x, z}
      and {x, z} -> 1; negedge fires on 1 -> {0, x, z} and {x, z} -> 0; x <-> z is `kChangeOnly`.
      `kBothEdges` (the `edge` keyword) matches either posedge or negedge. Covers
      `processes/edge_trigger_bit_select` (whole-variable edges) and the edge subset of
      `processes/event_triggers`.
- [x] T4 -- Event control event lists `@(posedge clk or negedge rst_n)` / comma form. A single
      `EventControl` carries multiple `EventTrigger` entries; the runtime races them and re-arms
      whichever signal won the wake-up. Covers the event-list subset of `processes/event_triggers`.
- [ ] T5 -- Edge trigger on bit-select / range-select (`@(posedge bus[2])`). Needs `mir::SelectExpr`
      from `operators.md` W4 / W5. Closes `processes/edge_trigger_bit_select` (selected subset) and
      `processes/edge_trigger_dynamic`.

### Synchronisation primitives

- [x] P9 -- Named events (LRM 15.5): `event e;` declaration, `-> e;` trigger, `@e;` await,
      `e.triggered` query. HIR carries `EventTriggerStmt` for `-> e;` and
      `TimingControl::NamedEventControl` for `@e;`; HIR -> MIR collapses both onto method calls on
      the `lyra::runtime::NamedEvent` data type, which records `last_triggered_at_` (LRM 15.5.3
      same-time-step persistence falls out of `last_triggered_at_ == services.Now()`) and wakes its
      waiter list. `e.triggered` returns a 1-bit `PackedArray`. `->>` non-blocking trigger,
      `wait_order(...)`, event aliasing / nullness / comparison are out of scope.
- [x] P11 -- `wait (cond) body` level-sensitive control (LRM 9.4.3). HIR carries
      `WaitStmt {cond, body, sensitivity_list}`. Sensitivity is precomputed by driving slang's
      `DefaultDFA` on `cond` as a standalone expression during the AnalysisManager pass (see
      `docs/decisions/read-set-inference.md`); the AST -> HIR lowering looks it up keyed by the cond
      expression. HIR -> MIR lowers to `Block { While { !cond, SensitivityWait(reads) }; body }` --
      the LRM 9.4.3 "skip suspend if cond is already true" semantic falls out of the while-loop
      shape. `wait fork;` (9.6.1) and `wait_order(...)` (15.6) are out of scope. Closes
      `processes/wait_event`.

### Concurrency

- [ ] P8 -- `fork` / `join` / `join_any` / `join_none`. Needs concurrent-process scheduling: spawn
      child coroutines, parent suspends until the join condition is met. Distinct from every other
      procedural construct because the body produces multiple parallel threads. Deferred until the
      single-process surface (P10, P9, P11) is settled.

### Generate

- [ ] P12 -- Process generate (`generate` / `if generate` / `for generate` containing procedural
      blocks). Closes `processes/generate`. Largely a frontend elaboration concern; the lowered
      processes ride on the existing P1..P11 machinery.

## Out of Scope

- Scheduler-region behaviour (Active / Inactive / NBA / Observed / Reactive / Postponed). Each
  region's invariants are tracked under `scheduling/*` archive items; the per-region implementation
  lives in `src/lyra/runtime/engine.cpp` but the cross-region tests have no dedicated progress file
  yet. Create `scheduling.md` when that workstream becomes actionable.
- `disable` and `disable fork`. Tracked separately under process control (LRM 9.6).
- `expect` statement and `wait_order`. Verification-only constructs.
