# Processes

Tracks SystemVerilog procedural-block constructs and the supporting timing-control machinery. Covers
archive items under `archived/tests/sv_features/processes/`.

The numeric IDs (P1..P15, T1..T5) are stable references and do **not** imply execution order.

## Actionable

No procedural-block items are currently actionable from within this workstream. Open items wait on
machinery owned by other workstreams; see [Blocked](#blocked).

## Blocked

| Item   | Blocked on                                                                                                                                                       |
| ------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| P12    | Process generate; rides on the existing P1..P11 surface, mostly frontend elaboration.                                                                            |
| T2..T5 | Compound event expressions (concatenation, arithmetic, dynamic index): needs the snapshot + re-eval wrapper at HIR -> MIR plus a runtime edge-classifier helper. |

## Sub-Steps

### Procedural blocks

- [x] P1 -- `initial` (LRM 9.2.1). Runs once at time 0; finishes when the body completes. Coverage
      rides on every other test in the corpus.
- [x] P2 -- `final` (LRM 9.2.3). Drained at end of simulation. `$finish` inside `final` ends
      simulation immediately. Time-controlling statements inside `final` are rejected at the runtime
      boundary because they would suspend a coroutine the engine cannot resume.
- [ ] P14 -- `$finish` / `$stop` verbosity level (LRM 20.2). The level argument (0 / 1 / 2) is
      parsed and validated at lowering, but the engine terminates regardless of its value; the
      level-gated end-of-simulation reporting (nothing / time / time plus statistics) is not
      implemented.
- [x] P3 -- `always` / `always_ff` (LRM 9.2.2). `always_ff` collapses to the same shape as `always`
      because the LRM 9.2.2.4 restrictions are lint-only and the frontend already enforces them.
      Pathological zero-delay loops are caught by the engine's settle limit.
- [x] P10 / P13 -- `always_comb` / `always_latch` (LRM 9.2.2.2.1) and `always @*` / `always @(*)`
      (LRM 9.4.2.2). Slang's flow analysis produces the implicit read sets; the body runs at t = 0
      (always_comb / always_latch) or after the first wait (`@*`), then waits on any change to the
      read set. Reads collapse to whole-variable subscription until the runtime supports bit-level
      any-change.

### Variable lifetime

- [x] P15 -- Process-body variable lifetime (LRM 6.21). A local declared in an `initial` / `always`
      / `final` body follows its resolved lifetime. An automatic local is reinitialized on each
      entry and lives only for that activation; a static local (the module default) has one
      per-instance copy that is default-initialized once and persists across activations, so it
      stays live after the process body completes -- which is what lets a detached fork branch read
      it (`fork-join.md` FJ4). A bare declaration takes the module's static default. Static locals
      that share a name across sibling or nested blocks of one process are kept distinct. See
      `decisions/variable-lifetime-storage.md` for the storage rationale.

### Procedural assignments

- [x] P4 -- Non-blocking assignment `<=`. Submits a closure to the NBA region during the active
      phase; the engine commits all closures in the NBA commit phase before observing changes.
      Re-entrancy during NBA commit is rejected.
  - [ ] A non-blocking assignment whose target is an automatic-lifetime procedural local (LRM
        13.3.2) is rejected; the deferred-commit closure would outlive the local's activation.
- [x] P7 -- Continuous assignment `assign x = y;` (LRM 10.3). Read set is precomputed by slang's
      flow analysis (LRM 10.3.2 reads identical to LRM 9.2.2.2.1). Drive strength (LRM 10.3.4) and
      `assign #N` (LRM 10.3.3) are diagnosed; concat LHS rides on the procedural side.
  - [ ] A delay on a continuous assignment (`assign #N x = y;`, LRM 10.3.3) is rejected; the net's
        drivers update without the specified inertial / transport delay.
  - [ ] A continuous-assignment form beyond the plain `assign` and net-declaration assignment (the
        remaining LRM 10.3 forms) is rejected.
  - [x] The pure value queries in a continuous-assignment right-hand side: a time read (`$time` /
        `$stime` / `$realtime`, LRM 20.3, scaled to the scope's time unit per LRM 3.14.2), a
        plusargs test (LRM 21.6), and `$sformatf` (LRM 21.3.3). Each reads state and sequences
        nothing, so it needs no process body and lowers through the same handler the procedural side
        uses. The system subroutines that are effects remain rejected there; their output-bearing
        siblings never arrive, since the frontend keeps an output argument out of a structural
        context.

### Timing controls

- [x] T1 -- Delay control `#N` (LRM 9.4.1). Engine schedules the suspended coroutine onto the
      delayed queue.
- [x] T2..T5 -- Event control `@(...)` across all `expression`, `posedge`, `negedge`, `edge` forms,
      including bit-select (`bus[N]`), range-select (`bus[hi:lo]`), and indexed part-select
      (`bus[base +: w]` / `bus[base -: w]`) on packed types, plus event-list (`or` / `,`) of any of
      the above. The pipeline routes every value-change wait (always_comb body, `@*`, `wait (cond)`,
      `@(...)`) through `mir::SensitivityWaitStmt` carrying per-leaf `(var, bit_range, edge_kind)`
      records; slang's flow analysis computes the leaves and the AST -> HIR lowering attaches the SV
      edge identifier (with LRM 9.4.2 LSB-reduce for edge-qualified single-leaf expressions).
      Runtime per-leaf classification samples each waiter's projection on every variable write, so
      changes outside the projection do not cause spurious wakes (LRM 9.4.2 "no change in result"
      rule). `ClassifyEdge` covers the LRM Table 9-2 4-state transition matrix; `kBothEdges` matches
      either posedge or negedge. Covers `processes/event_triggers` and
      `processes/edge_trigger_bit_select` for the packed-vector, constant-selector, single-leaf
      subset, including multi-dimensional packed selects and ascending / negative-base ranges (LRM
      11.5.1 direction translation). Compound expressions (concatenation, arithmetic, dynamic index)
      remain rejected -- see Blocked.
  - [ ] The `iff` qualifier on an event control (LRM 9.4.2.3): `@(e iff cond)` is rejected.
  - [ ] An edge event control on a non-packed-bit-vector operand, and a value-change event control
        on a non-value operand (LRM 9.4.2): only packed-vector / value operands are accepted.
  - [ ] Repeated event control `repeat (N) @(...)` (LRM 9.4.4), and nested timing controls inside an
        event-list entry: only signal events compose in a list today.
  - [ ] A delay whose duration is not an integer or time literal (LRM 9.4.1): a computed or
        real-valued delay expression is rejected.
- [ ] T6 -- Event-trigger forms beyond the blocking `-> e`: the non-blocking trigger `->> e` (LRM
      15.5.2) and a delayed trigger carrying an intra-assignment timing control are rejected.

### Synchronisation primitives

- [x] P9 -- Named events (LRM 15.5): `event e;` declaration, `-> e;` trigger, `@e;` await,
      `e.triggered` query (LRM 15.5.3 same-time-step persistence). `->>` non-blocking trigger,
      `wait_order(...)`, event aliasing / nullness / comparison are out of scope.
- [x] P11 -- `wait (cond) body` level-sensitive control (LRM 9.4.3). Sensitivity is precomputed by
      slang's flow analysis on `cond` as a standalone expression. The "skip suspend if cond is
      already true" semantic falls out of the lowering. `wait fork;` (LRM 9.6.1) is a distinct
      process-control construct tracked in `fork-join.md`; `wait_order(...)` (LRM 15.6) is out of
      scope.

### Concurrency

- [ ] P8 -- `fork` / `join` / `join_any` / `join_none` (LRM 9.3). Spawns concurrent processes; the
      parent resumes per the join condition. Now tracked in its own workstream (`fork-join.md`),
      unblocked: the single-process timing surface (T1, T2..T5, P9, P11) is settled.

### Generate

- [ ] P12 -- Process generate (`generate` / `if generate` / `for generate` containing procedural
      blocks). Largely a frontend elaboration concern; the lowered processes ride on P1..P11.

## Out of Scope

- Scheduler-region behaviour (Active / Inactive / NBA / Observed / Reactive / Postponed). Each
  region's invariants are tracked under `scheduling/*` archive items; create `scheduling.md` when
  that workstream becomes actionable.
- `disable` and `disable fork` (LRM 9.6). Tracked separately under process control.
- `expect` statement and `wait_order`. Verification-only constructs.
