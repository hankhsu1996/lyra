# Processes

Tracks SystemVerilog procedural-block constructs and the supporting timing-control machinery.

The numeric IDs (P1..P20, T1..T7) are stable references and do **not** imply execution order.

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
      What a final procedure may contain is what a function may, so none can consume time. Work
      still suspended when the run ends stops where it stands: its frame is released without
      resuming, so what it would have written is not written, and a cleanup that frame was holding
      open runs with nothing of the design executing and has nothing left to withdraw from.
- [x] P14 -- `$finish` / `$stop` finish number (LRM 20.2, Table 20-1), which `$fatal` takes first
      (LRM 20.10). The argument selects what the tool prints where the task is reached -- nothing,
      the call's location with the simulation time, or those plus what the run has cost -- and
      defaults to 1. It is any expression, read when the call runs, and a value other than 0, 1 or 2
      prints as 1, an unknown bit reading as 0. The message is also the whole of what tells `$stop`
      from `$finish` in a run nothing can resume, since neither the ending nor the exit status
      differs.
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
- [x] P17 -- An ending task written inside a function. `$finish`, `$stop` and `$fatal` run there as
      they do anywhere: the run ends and the calling execution departs, so nothing after the call
      runs -- not the rest of the function, not the functions and methods it was called through, and
      not the procedure around them. `$exit` does there what it does anywhere: from a thread of a
      program it ends that program, the calling execution with it, and from any other thread it is
      ignored. LRM 13.4's restriction does not reach them: it is a closed list of time-controlling
      statements -- `#`, `##`, `@`, the `fork-join` family, `wait`, `wait fork`, `wait_order`,
      `expect` -- and rule (c) of the same clause positively admits a function that kills the
      current process, which is no less final. It is the natural shape of a checking helper: a
      severity task under an `if`, called from several places. An exported function may end the run
      too: the foreign frames beneath it are in the disabled state of LRM 35.9, as under a kill, and
      return on their own, and a further call into an export is reported and runs nothing.
- [x] P3 -- `always` / `always_ff` (LRM 9.2.2). `always_ff` collapses to the same shape as `always`
      because the LRM 9.2.2.4 restrictions are lint-only and the frontend already enforces them.
      Procedures that keep waking each other with no delay between them (LRM 9.2.2, 12.7.6) end the
      run at a bound on one time slot's passes, with a report naming each procedure the last passes
      ran by where it is written. A procedure that never stops to wait is not ended, since it cannot
      be told from one taking long: a run stopped from outside says the time and the procedure it
      was in. Not yet: the statement inside that procedure, which needs a source place below a body.
- [x] P18 -- What starts at time zero starts in turn (LRM 4.7 leaves the order open, 9.2 says
      `initial` need not come before `always`): every `always` procedure, then every continuous
      assignment and port connection, then every `initial` procedure, then every `always_comb` and
      `always_latch` (LRM 9.2.2.2). So an `always` waiting on a name sees the first value a
      continuous driver gives it, as an edge where it is one, and a register whose only reset is
      tied to a constant is reset; and an `initial` reads what a constant drives. An `initial` that
      itself waits for a driver's first value does not see it.
- [x] P10 / P13 -- `always_comb` / `always_latch` (LRM 9.2.2.2.1) and `always @*` / `always @(*)`
      (LRM 9.4.2.2). The block's own text is analyzed, and each function an `always_comb` or
      `always_latch` calls reports what it reads and writes once, where the process is created,
      without running -- so a function counting its own calls counts one for the run at time zero
      and one for each run after; the body runs at t = 0 (`always_comb` / `always_latch`) or after
      the first wait (`@*`), then waits on any change to the read set. Such a procedure, and one
      waiting at an event control it opens with (`always_ff`), answers process control as any
      process does (LRM 9.7): it reports WAITING between runs, misses a change while suspended, runs
      on resuming where a change had already made it ready, and runs no more once killed.

      What a read in that set contributes follows from what the name denotes, and every kind of
      declaration says so rather than sharing one answer. A net or a variable is waited on wherever
      its cell lives. A static class property is one too -- the one copy its declaring scope keeps,
      usable with no object of that type: one for a class a package declares, one per instance of a
      module that declares it (LRM 8.9, 6.22). A name a view offers contributes what the interface says
      it reads (LRM 25.5.4). A value fixed before simulation starts contributes nothing, which a
      parameter, an enumeration name and a specparam each are. LRM 9.2.2.2.1 excludes a reference
      through a class object and a variable the block itself declares, so an instance property and
      `this` contribute nothing either. `@*` has no such exclusion, but an automatic variable its
      statement declares -- a `for` loop's variable, a block's local -- does not exist while the
      control waits and nothing outside can write it, so it contributes nothing; a static one the
      statement declares is waited on. A variable the statement uses only to control its `for` loops
      contributes nothing either, though LRM 9.4.2.2 lists it: read to the letter, two `@*`
      procedures each running a loop over one variable declared outside both resume each other
      without end, and real designs hold that shape. Nothing writes a foreach index, an array
      method's iterator or a pattern's binding while a wait stands, so no wait of any form
      subscribes to one. **A read whose kind this compiler does not carry, or whose storage it
      cannot reach from the reading scope, is refused by name** -- a subscription is never
      quietly left out, because a process that does not wake gives a wrong answer with nothing to
      see.

  - [x] A read of one element of an unpacked array of fixed size, or one member of an unpacked
        structure, to any depth and down to bits of a packed element, waits on that part alone, as a
        read of a bit range of a packed value does: a write to a sibling part does not wake it, and
        a part the block also writes is left out of its list (LRM 9.2.2.2.1 b). Two `always_comb`
        procedures may therefore each give a different element of one array a default and then
        override it (LRM 9.2.2.2).
  - [ ] Two reads still wait on more than the standard's list holds, each running a procedure that
        then reaches the same answer: an element of a queue, a dynamic array or an associative array
        at a constant index waits on its whole container; and a part of an unpacked variable read
        only in a way no single select spells -- what is left of a read once part of it is written
        -- waits on the whole variable.
  - [ ] A whole assignment that changed one element of an unpacked variable wakes every wait on a
        part of it, and a slice assignment every wait on an element inside the slice, changed or
        not.
  - [ ] A part more than six selects down is waited on as the part six selects down that holds it,
        so a write to a sibling beside it wakes its reader.
  - [ ] A write to some bits of a packed value asks every wait on that value whether it reads them,
        so its cost follows how many procedures wait on the value and not how many read the bits
        written. Not measured on a wide vector with many readers.
  - [x] An `always_comb` or `always_latch` calling a function declared outside its own instance --
        reached through an interface port, by a name climbing out of the instance, down into a
        child, or in a package -- wakes on what that function reads, and on what the functions it
        calls read in turn. Each instance of a module wakes on what its own call reaches. What the
        block or those functions write is left out, so a nonblocking write the block makes to a
        variable its function reads does not run it again.

### Variable lifetime

- [x] P16 -- Process-body variable lifetime (LRM 6.21). A local declared in an `initial` / `always`
      / `final` body follows its resolved lifetime. An automatic local is reinitialized on each
      entry and lives only for that activation; a static local (the module default) has one
      per-instance copy that is default-initialized once and persists across activations, so it
      stays live after the process body completes -- which is what lets a detached fork branch read
      it after detaching. An automatic local a detached fork branch still reads outlives the scope
      that declared it, whether a declaration statement declares it or a `foreach` does as its loop
      variable (LRM 12.7.3). A bare declaration takes the module's static default. Static locals
      that share a name across sibling or nested blocks of one process are kept distinct. A
      subroutine's formal arguments follow the same rule: those of a static task or function are one
      variable each, shared by every call in progress and keeping what the last call left in them
      (LRM 13.3.2, 13.4.2), while an automatic subroutine's are each call's own. See
      `decisions/variable-lifetime-storage.md` for the storage rationale.

### Procedural assignments

- [x] P4 -- Non-blocking assignment `<=`. Reads its right-hand side where the statement is reached
      and leaves the update for the NBA region of that time slot, so the procedure carries on and
      every such update in the slot lands together, before anything the slot goes on to observe. An
      update that itself schedules work leaves it for a later pass of the same slot (LRM 10.4.2).
  - [ ] A non-blocking assignment whose target is an automatic-lifetime procedural local (LRM
        13.3.2) is rejected; the deferred-commit closure would outlive the local's activation.
- [x] P7 -- Continuous assignment `assign x = y;` (LRM 10.3). Read set is what the right-hand side
      reads (LRM 10.3.2 reads identical to LRM 9.2.2.2.1). Drive strength (LRM 10.3.4) is honoured
      on a net target; concat LHS rides on the procedural side.
  - [x] A delay on a continuous assignment, written on the `assign` or on a net declaration that
        assigns (LRM 10.3.3). Each value the right-hand side takes reaches the target that long
        after the change, and one replaced by a different value before then never arrives, so a
        pulse shorter than the delay does not propagate; a change to the value already on its way
        does not put it off. Given rise, fall and turn-off delays, a scalar net takes the one Table
        28-9 names for the value changed to and a vector net the one 10.3.3's rule names. Each
        amount is read when the change happens, integral or real, and a zero delay lands in the
        Inactive region of the same time slot (LRM 4.4.2.3).
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
      list. A variable naming an object is waited on for which object it names: LRM 9.4.2 makes a
      write to a simple object handle an event whenever what it names is not what it named before,
      and a write naming the object already named is no event. The clause's own example places a
      wait on a property of the object beside a wait on the handle to say they are two different
      waits, and a write to the property resumes neither the other. The same holds of a chandle
      variable, whose value is the pointer it holds, and of a virtual interface, whose value is the
      instance it holds. A variable a call or a block declares -- a local, a loop variable, a formal
      of any direction -- is waited on as a module's is, and a write to it from a branch the body
      forks or through a reference it lent wakes the wait; a `ref` formal is waited on as whatever
      its actual is, a variable, an element, a member or a class property (LRM 6.21, 13.5.2).
- [x] A wait follows what its expression reached (LRM 9.4.2). A property of a class object --
      reached through a handle, the standard's own `@(p.status)`, through a handle another property
      holds, or named bare inside a method -- is watched as its object, which every write to any of
      its properties reports to: an assignment of any form, a nonblocking one when it lands, a
      built-in method that changes it, and a task writing through a `ref` bound to it. A variable
      reached through a virtual interface is watched in the instance the handle holds. Which object
      or instance that is follows the handle: writing the handle while the wait is under way
      reevaluates the expression, which is an event exactly when what it reaches now already
      differs, and a write to what it reached before no longer reaches it. This holds of an event
      control and of `wait (cond)` alike.
- [x] A wait follows what the functions its expression calls read (LRM 9.4.2), wherever they are
      declared: a variable of the module or a package, an element, a static property, a property of
      the object a method runs on or of one a handle argument or a module's handle names, a variable
      of the instance a virtual interface holds, and what a function it calls reads in turn -- a
      method, a static method, a package function, an interface's function and one reached by a
      hierarchical name alike, virtual or not. Before this, a function reading a module variable
      left the wait unwoken on both backends, and a method call was refused. What a function reads
      is what the evaluation the wait makes read while running it, so an object or a virtual
      interface it reaches only through a variable of its own -- walking a list, recursing, an array
      method's `with` clause -- is followed exactly, and a handle it tests for null is not followed
      while it is null. A variable handed by `ref` to a function that reads it, what a constructor
      the function runs reads (its body, its properties' initializers, its base's arguments, written
      in `super.new` or on the extends specifier), and what a context DPI import reads through the
      exports it calls (LRM 35.5.3) are followed too; before this each left the wait unwoken.
      Finding out what a function reads is the evaluation itself, so a `$urandom` or `$random(seed)`
      inside one draws once per evaluation, where it drew twice. A system function answering from a
      dynamically sized value's current extent -- `$bits`, `$size`, `$high` -- reads that value, in
      a wait, `@*` and `always_comb` alike (LRM 20.6.2, 20.7); before this none of them ever woke.
- [x] A wait's expression and its `iff` qualifier are evaluated by the waiting process (LRM 4.5):
      once where the wait begins and once per change that reaches it, and a call in them runs as
      that process -- `process::self()` there answers the waiter. What that evaluation writes is no
      event for the waiting process, so a method whose call writes the object it waits on, a
      qualifier writing what it gates and one triggering the event it qualifies all leave the wait
      waiting; before this the first two overflowed the stack and the third ran once per nested
      trigger, on both backends. A null handle the expression reads through fails as the waiting
      process, after the write that nulled it has finished.
  - [ ] A value-change event control on any other operand whose change is not watched (LRM 9.4.2):
        an operand that is no value at all, and one whose value nothing yet compares.
- [x] T6 -- The non-blocking event trigger `->> e` (LRM 15.5.1), with and without a control. The
      statement runs without waiting, and the trigger becomes an update due in the nonblocking
      assignment region of a slot: this one where the source wrote no control, the one a delay
      names, or the one an event happens in, in every form an event control takes including a repeat
      count. A procedure that only reaches the wait later in that slot is still in time for the
      trigger, since the trigger has not happened when the statement runs. What it triggers is the
      object the event names where the statement stands, whatever the event names when it lands. The
      standard makes no process of such an update, so `wait fork` does not wait for one.
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
      before it. The control starts watching where the statement stands, so a change the procedure
      makes straight after the statement is an event for it, and an event it names is the object
      that event names there. Everything the update needs is settled where the statement stands, its
      target's index included, and two updates to one variable land in the order the statements ran.
      A concatenation left-hand side (LRM 11.4.12) is one left-hand side, so a control on such an
      assignment is read once and every member's share lands in the same slot. The standard makes no
      process of a pending update, so `wait fork` does not wait for one and `disable fork` does not
      reach one.

### Synchronisation primitives

- [x] P9 -- Named events (LRM 15.5). An event variable is a handle to a synchronization object (LRM
      6.17): a declaration with no initializer names a new one -- each element of a fixed-size
      array, each element `new[N]` makes and each member of a structure included -- and `null` names
      none, which is what reading an entry an array does not have answers (Table 7-1). Assigning one
      event to another makes both name one object, so a trigger through either reaches whoever waits
      on either (LRM 15.5.5.1); two events compare by the object they name, and an event tests true
      exactly where it names one (LRM 15.5.5.3). An event is held wherever a value is -- an element
      of every kind of array, a member of a structure, a property of a class, a variable of a
      package, a message of a mailbox, an argument of any direction and a result -- and is triggered
      (`->`) and waited on (`@`), alone or as an entry of an event list beside value changes,
      however it is reached. A wait is put on the object its event names where the wait begins and
      stays there, so an assignment reaches only later waits and naming another element of an array
      is no occurrence (LRM 15.5.1); the object lives as long as anything names it or waits on it
      (LRM 15.5.5.2). Triggering an event that names nothing does nothing, and a wait on one waits
      for as long as the run lasts, one of the two outcomes the clause leaves open. `e.triggered`
      (LRM 15.5.3) is the object's state for the time step, which a `wait` condition may read, so a
      procedure unblocks whether it reaches the wait before the trigger or in the same time step as
      it, and stays blocked if it reaches one in a later step. An `iff` qualifier holds a trigger
      back as it holds back a value change.
  - [ ] `wait_order(...)` (LRM 15.5.4), refused where it is written.
- [x] P11 -- `wait (cond) body` level-sensitive control (LRM 9.4.3). Sensitivity is what `cond`
      reads. The "skip suspend if cond is already true" semantic falls out of the lowering.
      `wait fork;` (LRM 9.6.1) is a distinct process-control construct belonging to the fork
      surface; `wait_order(...)` (LRM 15.5.4) is out of scope.
- [x] P21 -- `semaphore` and `mailbox #(T)` (LRM 15.3, 15.4): every method, on both backends, and a
      class may extend either (LRM 15.2). A process that has to wait is served in the order it
      arrived, so a request for more keys at the head holds back a later one for fewer, a waiter
      that stops waiting -- killed, or its call left as a block around it is disabled -- holds back
      nobody, and a `try_` form succeeds only where its waiting form would not wait. A negative key
      count is reported as an error and changes nothing; a negative mailbox bound is warned of and
      holds any number of messages. A message may be of any type, a class handle or a type a design
      element declares included.
  - [x] A `mailbox` with no type given for its messages (LRM 15.4.9): each message keeps the type it
        was put with, and is received only into a variable of an equivalent type (LRM 6.22.2) -- a
        queue whatever bound either side declares -- a `get` or `peek` into another reporting an
        error and a `try_` form answering below zero, the message staying either way.

### Concurrency

- [x] P8 -- `fork` / `join` / `join_any` / `join_none` (LRM 9.3). Spawns concurrent processes; the
      parent resumes per the join condition, once enough branches have terminated -- a branch killed
      counts as surely as one that ran out its statements.
  - [ ] A `join_none` reached while no process is running -- a class constructor that forks, called
        from a variable's declaration initializer, which runs before any process starts (LRM 6.21).
        It is reported as a compiler bug. Whether the standard gives such a fork a parent, or makes
        the program illegal, has not been read; either way the answer is not an internal error.

- [x] P19 -- A program block (LRM 24) runs as a program. Its initial procedures, its continuous
      assignments and the connections of its ports run in the reactive region set, and so does every
      thread they start and every subroutine those threads call, wherever it is declared: a
      resumption lands in Reactive, a `#0` in Re-Inactive and a nonblocking assignment in Re-NBA
      (LRM 24.3.1, 24.5). Once a program's initial procedures have all ended it ends whatever they
      started, and once every program has ended the run ends by an implicit `$finish`, with `final`
      procedures run (LRM 24.3). `$exit` ends the program the calling thread originates in, and from
      any other thread is ignored with a warning (LRM 24.7). A program nested in a module or an
      interface uses its variables, events and subroutines by the plain name, so programs nested in
      one scope share them (LRM 24.3).
- [x] P20 -- An anonymous program (LRM 24.6) declares its subroutines and classes in the package or
      `$unit` scope around it, and a program reaches them by the scope's name, by import or as
      `$unit` items; a task of one called from a program runs in the program's thread, so in the
      reactive region set (LRM 24.5). That only programs may name them is the front end's check.

### Generate

- [ ] P12 -- Process generate (`generate` / `if generate` / `for generate` containing procedural
      blocks). Largely a frontend elaboration concern; the lowered processes ride on P1..P11.

## Out of Scope

- `disable` and `disable fork` (LRM 9.6). Tracked separately under process control.
- `expect` statement and `wait_order`. Verification-only constructs.
