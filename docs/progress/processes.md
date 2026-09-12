# Processes

Tracks SystemVerilog procedural-block constructs and the supporting timing-control machinery.

The numeric IDs (P1..P16, T1..T7) are stable references and do **not** imply execution order.

## Actionable

Every numbered item is closed but P12. What stays open otherwise is the forms recorded as rejected
under each item, and the conformance gaps at the end.

## Blocked

| Item | Blocked on                                                                            |
| ---- | ------------------------------------------------------------------------------------- |
| P12  | Process generate; rides on the existing P1..P11 surface, mostly frontend elaboration. |

## Sub-Steps

### Procedural blocks

- [x] P1 -- `initial` (LRM 9.2.1). Runs once at time 0; finishes when the body completes. Coverage
      rides on every other test in the corpus.
- [x] P2 -- `final` (LRM 9.2.3). A final procedure occurs at the end of simulation time, which a run
      reaches by being asked to end and equally by running out of work. A further request to end,
      made from inside one, ends the simulation immediately, so the ones still queued do not run.
      What a final procedure may contain is what a function may, so none can consume time.
- [x] P14 -- `$finish` / `$stop` / `$exit` diagnostic level (LRM 20.2, Table 20-1). The argument
      selects what the tool prints where the task is reached -- nothing, the call's location with
      the simulation time, or those plus what the run has cost -- and defaults to 1. The message is
      also the whole of what tells `$stop` from `$finish` in a run nothing can resume, since neither
      the ending nor the exit status differs.
  - [ ] The most verbose level reports the processor time the run has cost and not the memory, which
        the clause names alongside it. Nothing standard answers for memory, so a figure needs a
        per-platform query in the library an emitted program links.
- [x] P15 -- A run-time error Lyra raises for the design ends the simulation the way `$fatal` does.
      It is reported at the severity chosen for that condition, naming the scope it was raised in
      and the simulation time, and the run then walks the ending every other ending walks -- so
      final procedures run, immediate cover results are reported, and buffered output is flushed.
      Time-zero initialization is inside the simulation, so an error in a variable initializer is
      one of these rather than something that escapes past the host boundary. Which severity each
      LRM run-time error takes stays open per condition; what is fixed is that a condition the
      standard leaves unstated is fatal, and that a report which does not end the run can be made.
- [ ] P17 -- An ending task written inside a function. `$finish`, `$stop`, `$exit` and `$fatal` are
      refused there, because ending the run is carried as an execution that does not continue while
      a function is compiled as one that always returns to its caller. LRM 13.4's restriction does
      not reach them: it is a closed list of time-controlling statements -- `#`, `##`, `@`, the
      `fork-join` family, `wait`, `wait fork`, `wait_order`, `expect` -- and rule (c) of the same
      clause positively admits a function that kills the current process, which is no less final. So
      this is legal SystemVerilog, and it is the natural shape of a checking helper: a severity task
      under an `if`, called from several places, which is how a self-checking design states its
      expectation once instead of at every call site. It surfaces as a host compile error against
      generated code rather than as a diagnostic naming the construct, which is a second gap, and
      the one that makes it read as a compiler fault.
- [x] P3 -- `always` / `always_ff` (LRM 9.2.2). `always_ff` collapses to the same shape as `always`
      because the LRM 9.2.2.4 restrictions are lint-only and the frontend already enforces them.
      Pathological zero-delay loops are caught by the engine's settle limit.
- [x] P10 / P13 -- `always_comb` / `always_latch` (LRM 9.2.2.2.1) and `always @*` / `always @(*)`
      (LRM 9.4.2.2). Slang's flow analysis produces the implicit read sets; the body runs at t = 0
      (always_comb / always_latch) or after the first wait (`@*`), then waits on any change to the
      read set.

      What a read in that set contributes follows from what the name denotes, and every kind of
      declaration says so rather than sharing one answer. A net or a variable is waited on wherever
      its cell lives. A static class property is one too -- LRM 8.9 makes it the single copy a class
      shares, usable with no object of that type -- and the class may be declared inside the module
      or outside every design unit alike. A name a view offers contributes what the interface says
      it reads (LRM 25.5.4). A value fixed before simulation starts contributes nothing, which a
      parameter, an enumeration name and a specparam each are. LRM 9.2.2.2.1 excludes a reference
      through a class object and a variable the block itself declares, so an instance property and
      `this` contribute nothing either. **A read whose kind this compiler does not carry, or whose
      storage it cannot reach from the reading scope, is refused by name** -- a subscription is never
      quietly left out, because a process that does not wake gives a wrong answer with nothing to
      see.

  - [ ] Sensitivity narrowed below whole-variable granularity for a read that is not a range of
        bits: a process reading one element or one field wakes on any write to the variable
        containing it, where one reading a bit range of a packed value already wakes only on a write
        reaching that range. Correctness is unaffected -- the process re-evaluates and reaches the
        same answer -- so this is what separates a correct wake set from a minimal one.

### Variable lifetime

- [x] P16 -- Process-body variable lifetime (LRM 6.21). A local declared in an `initial` / `always`
      / `final` body follows its resolved lifetime. An automatic local is reinitialized on each
      entry and lives only for that activation; a static local (the module default) has one
      per-instance copy that is default-initialized once and persists across activations, so it
      stays live after the process body completes -- which is what lets a detached fork branch read
      it after detaching. A bare declaration takes the module's static default. Static locals that
      share a name across sibling or nested blocks of one process are kept distinct. See
      `decisions/variable-lifetime-storage.md` for the storage rationale.

### Procedural assignments

- [x] P4 -- Non-blocking assignment `<=`. Reads its right-hand side where the statement is reached
      and leaves the update for the NBA region of that time slot, so the procedure carries on and
      every such update in the slot lands together, before anything the slot goes on to observe. An
      update that itself schedules work leaves it for a later pass of the same slot (LRM 10.4.2).
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

- [x] T1 -- Delay control `#N` (LRM 9.4.1). The amount waited is any numeric expression, evaluated
      where the statement is reached, so a parameter, a variable, and an arithmetic combination of
      them all name one; a later write to anything the expression read does not reach a wait already
      under way. The amount is expressed in the scope's time unit and rounded to its precision
      before use (LRM 3.14.1), which is what lets a real amount name a fraction of a unit. An
      unknown or high-impedance amount is no delay, and a negative one is its own bits read as an
      unsigned integer -- a wait no simulation reaches -- rather than an error.
- [x] T2..T5 -- Event control `@(...)` in every form: bare, `posedge`, `negedge` and `edge`, and an
      event list (`or` / `,`) of any of them. Every construct that waits on a signal -- an
      `always_comb` body, `@*`, `@(...)`, `wait (cond)`, and a continuous assignment -- waits the
      same way, on the variables the expression reads. What an event control watches is the value of
      that expression: a change to an operand that leaves the value alone is no event (LRM 9.4.2 "no
      change in the result" rule), and an edge is the direction its least significant bit took, over
      the whole LRM Table 9-2 transition matrix, with `edge` matching either direction. The
      expression need only reduce to a singular value (LRM 9.4.2), so it may read several variables
      -- a concatenation, an operator over them, a select whose index is itself read -- and its
      operand may be an element or a field of an unpacked aggregate as well as a packed select of
      any depth, direction or base, including an indexed part-select (LRM 11.5.1 direction
      translation). An edge reads any integral operand (LRM 6.11.1), an enumeration and a packed
      structure or union included, since each is one packed vector and the edge is its least
      significant bit -- so a move to a numerically smaller enumerator can be a posedge and a move
      to a larger one no edge at all. A procedure that is not waiting at the control has nothing
      watching there, so a change while it is elsewhere is not detected. An entry may carry an `iff`
      qualifier (LRM 9.4.2.3), which gates the event without gating the watching: it is read where
      the watched expression changes and never when the qualifier itself does, so a change in the
      qualifier alone reaches nothing, and a change it holds back still moves what the wait compares
      against next. It qualifies a named event as well, and binds tighter than the `or` of an event
      list.
  - [ ] A value-change event control on a non-value operand (LRM 9.4.2): only value operands are
        accepted.
  - [ ] A nested timing control inside an event-list entry: only signal events compose in a list
        today.
- [x] T6 -- The non-blocking event trigger `->> e` (LRM 15.5.1), with and without a control. The
      statement runs without waiting, and the trigger becomes an update due in the nonblocking
      assignment region of a slot: this one where the source wrote no control, the one a delay
      names, or the one an event happens in, in every form an event control takes including a repeat
      count. A procedure that only reaches the wait later in that slot is still in time for the
      trigger, since the trigger has not happened when the statement runs. The standard makes no
      process of such an update, so `wait fork` does not wait for one.
- [x] T7 -- Intra-assignment timing controls (LRM 9.4.5). An assignment carrying one reads its
      right-hand side where the statement is reached and makes the assignment only once the control
      is satisfied, so a later write to anything that right-hand side read does not reach it --
      which is what lets `fork a = #5 b; b = #5 a; join` swap the two. A blocking form suspends the
      procedure meanwhile and covers all three controls: a delay, an event control, and a repeat
      event control that waits for that many occurrences, where a count that is zero, negative,
      unknown or high impedance waits for none. A left side that needs evaluating is evaluated when
      the control is satisfied rather than where the statement is reached (LRM 10.4.1). A
      non-blocking delay does not suspend the procedure: it schedules the update into the NBA region
      of the slot that delay names, and several such updates to one variable stay pending at once,
      each landing at its own time (LRM 4.4.2.4, 10.4.2). A non-blocking event control does not
      suspend it either: the update becomes due in the NBA region of the slot the event happens in,
      however far off that is, so a procedure the same event wakes reads the value the target held
      before it. Everything the update needs is settled where the statement stands, its target's
      index included, and two updates to one variable land in the order the statements ran. A
      concatenation left-hand side (LRM 11.4.12) is one left-hand side, so a control on such an
      assignment is read once and every member's share lands in the same slot. The standard makes no
      process of a pending update, so `wait fork` does not wait for one and `disable fork` does not
      reach one.

### Synchronisation primitives

- [x] P9 -- Named events (LRM 15.5): `event e;` declaration, `-> e;` trigger, `@e;` await, and the
      `e.triggered` query (LRM 15.5.3 same-time-step persistence), which a `wait` condition may read
      -- so a procedure unblocks whether it reaches the wait before the trigger or in the same time
      step as it, and stays blocked if it reaches one in a later step. Waiting for an event and
      waiting for a value to change are the same wait, differing only in that a trigger carries no
      value, so an `iff` qualifier is the whole of what can hold one back. `wait_order(...)` and
      event aliasing / nullness / comparison are out of scope: each wants an event variable to be an
      assignable handle to a shared synchronization object, which is a question about values rather
      than about scheduling.
- [x] P11 -- `wait (cond) body` level-sensitive control (LRM 9.4.3). Sensitivity is precomputed by
      slang's flow analysis on `cond` as a standalone expression. The "skip suspend if cond is
      already true" semantic falls out of the lowering. `wait fork;` (LRM 9.6.1) is a distinct
      process-control construct belonging to the fork surface; `wait_order(...)` (LRM 15.5.4) is out
      of scope.

### Concurrency

- [x] P8 -- `fork` / `join` / `join_any` / `join_none` (LRM 9.3). Spawns concurrent processes; the
      parent resumes per the join condition.

### Generate

- [ ] P12 -- Process generate (`generate` / `if generate` / `for generate` containing procedural
      blocks). Largely a frontend elaboration concern; the lowered processes ride on P1..P11.

## Out of Scope

- Scheduler-region behaviour (Active / Inactive / NBA / Observed / Reactive / Postponed). Each
  region's invariants belong to a scheduling workstream; create `scheduling.md` when that workstream
  becomes actionable.
- `disable` and `disable fork` (LRM 9.6). Tracked separately under process control.
- `expect` statement and `wait_order`. Verification-only constructs.
