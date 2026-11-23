# A run-time failure is not an activation's outcome

Status: Accepted, 2026-09-01. Reverses the third alternative of an activation's outcome in
[`activation.md`](../architecture/activation.md) invariant 2, a failure.

## Requirement

What an activation settles carries only what the source language can read. SystemVerilog has no
exceptions. Nothing in it produces, observes or recovers from a failure, and even division by zero
gives `x` (LRM 11.4.3). A task's value reaches its actual at the return (LRM 13.5), and a departure
is landed by the region `disable` names (LRM 9.6.2); a failure has no reader. Kept as a third
outcome, it caused a silent wrong answer on the execution backend. A failure inside a called body
was stored as that body's outcome, and the caller, asking only whether the body had finished, read a
result nothing had written.

## Choice

- **An activation settles the value it produced or the departure a region lands, and nothing else.**
  A failure is represented as an outcome in no IR and no backend.
- **Failures are told apart by who has to act on them, not by when they are found.**

  | Kind                                             | Who acts              | What it is                      |
  | ------------------------------------------------ | --------------------- | ------------------------------- |
  | Lyra does not carry this construct out           | user asks for support | a statement of Lyra's own limit |
  | the design does something the LRM calls an error | user fixes the design | a severity report (LRM 20.10)   |
  | Lyra contradicts itself                          | a developer           | an abort, not a language event  |

- **A design's run-time error is a severity report, at a level Lyra chooses per condition.** The LRM
  often says "shall be an error" with no severity (a negative `new[]` size, LRM 7.5.1), and only
  `$fatal` ends the run (LRM 20.10), so the model allows a report that does not end the run, which
  rules out an unwind as its mechanism.
- **Every way of ending the run walks one tail** -- stop, run every `final` procedure, then stop
  (LRM 9.2.3, 20.10). A fatal error Lyra raises for the design goes through it, so a design's
  `final` blocks run however the error was noticed.
- **Lyra's own inconsistency does not run the design's shutdown.** Running `final` blocks on a state
  already known to be wrong runs design code on it, so an internal error aborts and asks for a
  report.

## Alternatives turned down

- **The failure as a third outcome, handed to the awaiting body**, as C++, C#, Python, JavaScript
  and Kotlin do. It fails an outcome every alternative of which has a reader. The failure rests on
  SystemVerilog having no construct that catches.
- **The failure carried in the completion value, as Rust and Go carry an error.** It fails the same.
  The failure rests on an error value earning its place by letting the consumer branch on it, which
  SystemVerilog cannot write. Its real lesson is kept elsewhere, where a departure is carried as a
  value and taken as an explicit edge
  ([`nonlocal_control_flow.md`](../architecture/nonlocal_control_flow.md)).
- **A check after every operation that can fail.** It fails keeping failure handling out of bodies
  that have nothing to do with it, and a uniform checked answer breaks every expression holding a
  call apart while no frame between can act on it.

## Consequences across layers

- No await site, on either backend, carries machinery for failures.
- How a run ends, and how a run-time error reaches that ending, is
  [`scheduling.md`](../architecture/scheduling.md)'s; the three exception types and who throws each
  are the project's error policy (`CLAUDE.md`).
