# Processes

Tracks SystemVerilog procedural-block constructs and the supporting timing-control machinery. Covers
archive items under `archived/tests/sv_features/processes/`. Each archive item checkbox in
`architecture-reset.md` is checked when its `*.yaml` cases reproduce on the current pipeline.

The numeric IDs (P1..P11) are stable references to archive items and do **not** imply execution
order. Items in the [Actionable](#actionable) section can be picked up next; items in
[Blocked](#blocked) wait on machinery owned by another workstream.

## Actionable

| Item | Notes                                                                                             |
| ---- | ------------------------------------------------------------------------------------------------- |
| P9   | Named event (`event e; -> e; @e;`). New runtime primitive (named event handle, trigger, await).   |
| P11  | `wait (expr)` level-sensitive control. Needs a re-arm primitive over the existing change channel. |

## Blocked

| Item   | Blocked on                                                                                                            |
| ------ | --------------------------------------------------------------------------------------------------------------------- |
| P5, P6 | Bit-select / range-select expressions in MIR (`operators.md` W4 / W5). Edge triggers on selected lvalues need them.   |
| P7     | `assign` continuous-assignment closures share infrastructure with `always_comb` (P10); start P10 first.               |
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
- [x] P10 -- `always_comb` / `always_latch`. `slang::analysis::AnalysisManager` runs once per
      compilation to compute the LRM 9.2.2.2.1 implicit sensitivity list (longest static prefix of
      each read, excluding block-local declarations, also-written variables, and timing-control-only
      identifiers; reads inside called functions are tracked). The resulting per-procedure
      `ValueSymbol*` lists ride through `LowerCompilationFacts` into AST -> HIR, where they are
      translated into `hir::SensitivityEntry` (StructuralVarRef + TypeId) and attached to
      `hir::Process::implicit_sensitivity_list`. HIR -> MIR appends a synthesised `@(read_set)` tail
      wait inside the existing `forever` body and emits `ProcessKind::kInitial` so the engine
      schedules it on the active queue at time 0 (LRM 9.2.2.2: "automatically triggered once at time
      zero"). `always_latch` lowers identically to `always_comb` (LRM 9.2.2.3: execution-equivalent;
      only lint policy differs). Empty sensitivity lists (e.g. `always_comb     c = 7;`) degenerate
      to once-at-time-zero. Select expressions in reads collapse to whole-variable subscription
      until the runtime supports bit-level any-change. The single-driver restriction (LRM 9.2.2.2)
      and forbidden-construct checks (no blocking timing, no fork-join) are enforced by slang's
      frontend.

### Procedural assignments

- [x] P4 -- Non-blocking assignment `<=`. Submits a closure to the NBA region during the kActive
      phase; the engine commits all closures in the kCommitNba phase before observing changes.
      Re-entrancy (`SubmitNba` during NBA commit) is rejected. Covers the basic NBA cases used by
      every `always_ff` example.
- [ ] P7 -- Continuous assignment `assign x = y;`. Module-level construct, not a procedural block;
      shares the read-set inference path with P10. Closes `processes/continuous_assign`.

### Timing controls

- [x] T1 -- Delay control `#N`. `mir::TimedStmt` with `DelayControl{duration}`; the engine schedules
      the suspended coroutine onto the delayed queue. Covers `processes/delay`.
- [x] T2 -- Event control `@(x)` any-change. `mir::EventControl{triggers: [{signal, kAnyChange}]}`.
      `Var<T>` wrapper compares old vs new values and notifies the engine on any inequality. Covers
      `processes/event_triggers` (any-change subset).
- [x] T3 -- Event control edge polarity `@(posedge clk)` / `@(negedge clk)`.
      `EventEdge::{kPosedge,     kNegedge, kBothEdges, kAnyChange}` on each trigger;
      `Observable::TakeMatchingWaiters` filters by edge per LRM Table 9-2. Covers
      `processes/edge_trigger_bit_select` (whole-variable edges) and the edge subset of
      `processes/event_triggers`.
- [x] T4 -- Event control event lists `@(posedge clk or negedge rst_n)` / comma form. A single
      `EventControl` carries multiple `EventTrigger` entries; the runtime races them and re-arms
      whichever signal won the wake-up. Covers the event-list subset of `processes/event_triggers`.
- [ ] T5 -- Edge trigger on bit-select / range-select (`@(posedge bus[2])`). Needs `mir::SelectExpr`
      from `operators.md` W4 / W5. Closes `processes/edge_trigger_bit_select` (selected subset) and
      `processes/edge_trigger_dynamic`.

### Synchronisation primitives

- [ ] P9 -- Named events: `event e;` declaration, `-> e;` trigger, `@e;` await. New runtime type
      (named event handle, transient triggered state visible only within the current time slot per
      LRM 15.5.2). Closes `processes/named_event` and the trigger portion of
      `processes/event_triggers`.
- [ ] P11 -- `wait (expr)` level-sensitive control. Suspends the coroutine, subscribes to every
      signal read in `expr`, re-evaluates on any change, resumes when the expression becomes true.
      Distinct from `@(expr)` because it is level-sensitive (LRM 9.4.3): if `expr` is already true
      on entry, no suspension occurs. Closes `processes/wait_event`.

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
