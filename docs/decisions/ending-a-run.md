# Every way a run ends walks one path, and a report is not a stop

Date: 2026-09-08 Status: accepted

## Why this decision matters

A design carrying a `final` procedure can tell which way its run ended. `$fatal` runs every `final`
and exits; a run-time error Lyra raises for the same design reaches the tool's top frame without the
engine seeing it, so no `final` runs, no immediate cover result is reported (LRM 16.3), and a
`$write` still holding an unterminated record is lost.

[run-time-failure-is-not-an-outcome](run-time-failure-is-not-an-outcome.md) settled why: ending the
run means stop, run every `final`, then stop, and every way of ending it walks that tail. Its
Consequences named the tail as owed and not built there. This entry builds it, and in doing so has
to answer three things that record left open -- what a report is when it does not stop the run, what
`$stop` is when the tool has no interactive mode, and what remains able to leave a body once a
design's error no longer does.

## What the current shape produces

Four observable answers, each demonstrated on one file:

- A `final` procedure runs after `$fatal` and does not run after a negative `new[]` size (LRM
  7.5.1). One event, two behaviours, decided by who noticed it.
- `$finish(2)` prints nothing. The level is validated at lowering, folded into the call, carried to
  the runtime, and discarded there.
- `$stop` and `$finish` are indistinguishable, and nothing is left to tell them apart by: the one
  thing LRM 20.2 gives a tool to say which it was asked for is the diagnostic, and that is the
  message no level prints.
- A run-time error raised while variable initializers run leaves the host boundary entirely: the
  emitted program aborts on an uncaught exception, while the same class of error under the execution
  backend is reported by the command's own top frame. Two backends, two behaviours, again for one
  event.

## The model

Three primitives, and every ending is a composition of them.

```text
a report     severity, the call's file and line, the enclosing scope's hierarchical name,
             and the simulation time (LRM 20.10). Says something happened. Ends nothing.
a stop       the run is over, and whether it is a $finish (LRM 20.2).
a departure  this execution leaves, and no region may claim it. Ends one activation.
```

```text
$info / $warning / $error   report
$fatal                      report + stop + departure
$finish / $exit / $stop              stop
a design run-time error     report [+ stop + departure]  by its chosen severity
an internal inconsistency   the tool stops the run; none of the three, and no ending of the design's
```

The composition is not new. `$fatal` already lowers to a severity emit followed by a finish request,
which is LRM 20.10's "results in an implicit call to `$finish`" written out. What this entry does is
make everything else in the table use the same three pieces.

## The decisions

```text
D1. A report and a stop are separate operations, and a fatal is both. Nothing reports by stopping,
    and nothing stops by reporting.

D2. A report carries what LRM 20.10 requires of every severity task: the severity, the file and
    line of the call, the hierarchical name of the scope the call is made in, and the simulation
    time. Where in the design and when are properties of the report rather than of the call, so
    they are the reporting surface's to supply and every report carries them, including one Lyra
    writes for the design, which has no call site of its own to name.

D3. A design's run-time error is a report Lyra writes for the design, at a severity Lyra chooses
    per condition. A fatal one is D1's composition -- report, stop, depart -- and is not a failure
    that crosses the engine. Where the LRM leaves the severity unstated the condition is fatal,
    because a report that lets evaluation continue owes the expression a value, and only a
    condition whose value the standard defines has one to give.

D4. The departure is the unclaimable control effect the language layer already has: it names no
    region, so no region can claim it, and the activation is the only thing it can end. No control
    construct is added, and none means "a run-time error".

D5. What decides whether `final` procedures run is whether the simulation reached its end, not which
    task asked. A `final` procedure occurs at the end of simulation time (LRM 9.2.3), and the run
    exhausting its work, the design asking to end, and a run-time error of the design ending it are
    all that end. What is not is the tool being unable to carry on: a `final` procedure is design
    code, and the state it would read is already known to be wrong. What the tool owes either way --
    the immediate cover report (LRM 16.3), the output drain, the exit status -- is owed because the
    run is over, so it is not on this axis at all.

D6. `$stop` ends the run where a tool has no interactive mode, and the diagnostic is the whole of
    what separates it from `$finish`. LRM 20.2 has `$stop` suspend the simulation and `$finish` exit
    it; a tool that cannot resume a suspended run can only honour one of the two, and what it can do
    is say which it was asked for. So `$stop` runs the ending every other ending runs, prints its
    Table 20-1 diagnostic, and exits zero, because LRM 20.2 places it beside `$finish` and `$exit`
    rather than in the severity family and nothing there makes it an error.

D7. The diagnostic a stop prints is fixed by the level (LRM 20.2, Table 20-1) and is printed where
    the task executes rather than at the end of the run, because what level 1 prints is the location
    of the call and the time it was made, which is a fact of that moment. It goes to the diagnostic
    channel and is not a severity record: it states what the tool did, carries none of LRM 20.10's
    four levels, and happens once by construction, so it takes neither a severity label nor the
    per-site rate limit a severity report takes.

D8. The simulation's boundary is the elaboration phase boundary. Variable initialization and
    process activation are simulation activity at time zero (LRM 3.12, LRM 4), so an error raised
    there is a run-time error and takes D1's composition. An error raised while the design is built
    or its references resolved precedes the simulation, and is reported as such -- no `final`
    procedure exists yet to run, and none should.

D9. A body is left in exactly two ways: it returns, or it departs. A departure is carried outward
    until something claims it, and only two things can -- a region naming the target the departure
    names, or the activation's own landing. So an activation settles the value it produced or the
    departure that reached its landing, and a condition the source language cannot express is not a
    third outcome beside them: it is a departure no region can claim, which is the shape a kill
    already takes. Such a departure is reported at that landing, and the run ends there.
```

D9 completes the reversal
[run-time-failure-is-not-an-outcome](run-time-failure-is-not-an-outcome.md) began, and it takes a
different route than that record's D1 supposed. A body reached through a coroutine must hand an
escaping unwind to its promise, so an implementation cannot simply decline to carry one; what it can
decline is to call it a failure. Whether a region may claim a departure is the question every frame
between the raise and the landing asks, and that is what the two forms of one departure answer --
not whether the execution was in error.

## Rejected alternatives

- **Catch the thrown failure at the engine's loop and carry on into the tail.** The smallest change
  that runs `final` again, and it keeps three defects. The activation has already settled as having
  completed by the time the loop sees anything, which is a lie recorded in the state process control
  reads (LRM 9.7). A design's error travels to the engine as a failure the engine must know about,
  where the activation's own landing is the place that already reads how its body ended. And it
  answers nothing for an error raised before the loop exists, which is where the abort above comes
  from.

- **Run no `final` procedure after `$stop`.** It reads directly off LRM 9.2.3's sentence that a
  `final` procedure executes when simulation ends due to an explicit or implicit `$finish`, and that
  sentence does not bear the weight: a run that simply exhausts its work runs its `final` procedures
  too, and no `$finish` was called there either. The rule the clause states is that a `final`
  procedure occurs at the end of simulation time, and the `$finish` sentence names one way to reach
  it rather than the only one.

- **Give `$stop` a failing exit status.** It reads as the useful signal and the standard does not
  support it: only `$fatal` is defined to terminate with an error code (LRM 20.10), and `$stop` is
  not in that family. A tool that made it fail would report a design doing what it asked as an
  error.

- **Print the stop diagnostic at the end of the run.** For `$finish` the call and the ending are
  nearly the same moment, so it looks equivalent; it is not, because what level 1 has to print is
  where the call was made and when, and by the end of the run the tool would be reconstructing both
  from something it saved. Printing where the task executes needs nothing saved.

- **A fifth severity for the stop diagnostic.** The severity emit surface is LRM 20.10's, and a stop
  is not one of its four levels. Adding a value that is not a severity to the type that means
  severity buys a shared code path by making the type lie.

- **A check after every operation that can fail.** Rejected before for cancellation and again for
  failures, and the mid-expression case is what makes it tempting a third time: a guard inside an
  expression cannot suspend and has no value to return. It stays rejected for the same reason -- it
  puts the ending into every body that has nothing to do with it -- and the departure D4 names is
  what makes it unnecessary.

- **Aborting where an internal inconsistency is raised, to keep the stack.** The reader of one is a
  Lyra developer, and the raise site is what they want; a signal delivers it and an unwind has
  already lost it. It is rejected because the message that asks for a bug report, and the rendering
  that makes it legible, are worth more to that reader than a core file -- and because a run that
  aborts leaves its output unflushed, so the last thing the design printed before the compiler
  contradicted itself is the thing most likely to explain it.

## Consequences

- A bare `$finish` prints a line, because LRM Table 20-1's default level is 1. Every simulator does
  this; Lyra printing nothing was the level being discarded rather than a choice.
- `activation.md` invariant 2's third alternative goes. An activation settles the value it produced
  or the departure that reached its landing; what the two forms of a departure say is whether a
  region could have claimed it, and neither of them is a failure a consumer reads.
- A design's run-time error now ends the run the way `$fatal` does, so a `final` procedure runs and
  a `$write` still holding an unterminated record is flushed where neither happened before.
- A severity report gains two fields the standard requires and Lyra did not print, so every `$info`
  / `$warning` / `$error` / `$fatal` line changes shape.
- What each LRM run-time error's severity is stays open per condition, as before. D3 fixes the
  default and the reason for it; a condition whose value the standard defines may take a report that
  does not stop the run.

## Cross-references

- [run-time-failure-is-not-an-outcome](run-time-failure-is-not-an-outcome.md) -- the outcome model
  this completes, and the tail it named as owed.
- [conformance-diagnostic-claims](conformance-diagnostic-claims.md) -- how a requirement whose
  observable is a report is stated in the corpus, which is what makes D7 testable.
- `../architecture/scheduling.md` -- the engine that runs the tail.
- `../architecture/nonlocal_control_flow.md` -- the region, the raise and the cleanup D4's departure
  is an instance of.
- `../architecture/elaboration_lifecycle.md` -- the phases D8's boundary follows.
- IEEE 1800-2023: 3.12 and 4 (elaboration precedes simulation; initialization is time-zero
  activity), 9.2.3 (a `final` procedure occurs at the end of simulation time, which an explicit or
  implicit `$finish` reaches), 16.3 (immediate cover results reported at the end of simulation),
  20.2 and Table 20-1 (`$finish`, `$stop`, `$exit`, and the diagnostic each level prints), 20.10
  (the severity tasks, what every report includes, and `$fatal`'s implicit `$finish`).
