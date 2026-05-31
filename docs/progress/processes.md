# Processes

Tracks SystemVerilog procedural-block constructs and the supporting timing-control machinery. Covers
archive items under `archived/tests/sv_features/processes/`.

The numeric IDs (P1..P13, T1..T5) are stable references and do **not** imply execution order.

## Actionable

No procedural-block items are currently actionable from within this workstream. Open items wait on
machinery owned by other workstreams; see [Blocked](#blocked).

## Blocked

| Item | Blocked on                                                                                                       |
| ---- | ---------------------------------------------------------------------------------------------------------------- |
| T5   | Bit-select / range-select edge triggers; needs the selector machinery from `operators.md` W4 / W5.               |
| P8   | `fork` / `join_*`. Needs concurrent-process scheduling primitives; out of scope until single-process is settled. |
| P12  | Process generate; rides on the existing P1..P11 surface, mostly frontend elaboration.                            |

## Sub-Steps

### Procedural blocks

- [x] P1 -- `initial` (LRM 9.2.1). Runs once at time 0; finishes when the body completes. Coverage
      rides on every other test in the corpus.
- [x] P2 -- `final` (LRM 9.2.3). Drained at end of simulation. `$finish` inside `final` ends
      simulation immediately. Time-controlling statements inside `final` are rejected at the runtime
      boundary because they would suspend a coroutine the engine cannot resume.
- [x] P3 -- `always` / `always_ff` (LRM 9.2.2). `always_ff` collapses to the same shape as `always`
      because the LRM 9.2.2.4 restrictions are lint-only and the frontend already enforces them.
      Pathological zero-delay loops are caught by the engine's settle limit.
- [x] P10 / P13 -- `always_comb` / `always_latch` (LRM 9.2.2.2.1) and `always @*` / `always @(*)`
      (LRM 9.4.2.2). Slang's flow analysis produces the implicit read sets; the body runs at t = 0
      (always_comb / always_latch) or after the first wait (`@*`), then waits on any change to the
      read set. Reads collapse to whole-variable subscription until the runtime supports bit-level
      any-change.

### Procedural assignments

- [x] P4 -- Non-blocking assignment `<=`. Submits a closure to the NBA region during the active
      phase; the engine commits all closures in the NBA commit phase before observing changes.
      Re-entrancy during NBA commit is rejected.
- [x] P7 -- Continuous assignment `assign x = y;` (LRM 10.3). Read set is precomputed by slang's
      flow analysis (LRM 10.3.2 reads identical to LRM 9.2.2.2.1). Drive strength (LRM 10.3.4) and
      `assign #N` (LRM 10.3.3) are diagnosed; concat LHS rides on the procedural side.

### Timing controls

- [x] T1 -- Delay control `#N` (LRM 9.4.1). Engine schedules the suspended coroutine onto the
      delayed queue.
- [x] T2 -- Event control `@(x)` any-change. Compares old vs new on each write and notifies on any
      inequality.
- [x] T3 -- Event control edge polarity `@(posedge clk)` / `@(negedge clk)` / `@(edge clk)` (LRM
      Table 9-2). Full 4-state transition table: posedge fires on 0 -> {1, x, z} and {x, z} -> 1;
      negedge fires on 1 -> {0, x, z} and {x, z} -> 0; x <-> z is a change-only event. `edge`
      keyword matches either direction.
- [x] T4 -- Event control event lists `@(posedge clk or negedge rst_n)` and comma form. The runtime
      races the triggers and re-arms whichever signal won the wake-up.
- [ ] T5 -- Edge trigger on bit-select / range-select (`@(posedge bus[2])`). **Depends on**
      `operators.md` W4 / W5.

### Synchronisation primitives

- [x] P9 -- Named events (LRM 15.5): `event e;` declaration, `-> e;` trigger, `@e;` await,
      `e.triggered` query (LRM 15.5.3 same-time-step persistence). `->>` non-blocking trigger,
      `wait_order(...)`, event aliasing / nullness / comparison are out of scope.
- [x] P11 -- `wait (cond) body` level-sensitive control (LRM 9.4.3). Sensitivity is precomputed by
      slang's flow analysis on `cond` as a standalone expression. The "skip suspend if cond is
      already true" semantic falls out of the lowering. `wait fork;` (LRM 9.6.1) and
      `wait_order(...)` (LRM 15.6) are out of scope.

### Concurrency

- [ ] P8 -- `fork` / `join` / `join_any` / `join_none` (LRM 9.3). Spawns child coroutines; parent
      suspends until the join condition is met. Distinct from every other procedural construct
      because the body produces multiple parallel threads. Deferred until the single-process surface
      is settled.

### Generate

- [ ] P12 -- Process generate (`generate` / `if generate` / `for generate` containing procedural
      blocks). Largely a frontend elaboration concern; the lowered processes ride on P1..P11.

## Out of Scope

- Scheduler-region behaviour (Active / Inactive / NBA / Observed / Reactive / Postponed). Each
  region's invariants are tracked under `scheduling/*` archive items; create `scheduling.md` when
  that workstream becomes actionable.
- `disable` and `disable fork` (LRM 9.6). Tracked separately under process control.
- `expect` statement and `wait_order`. Verification-only constructs.
