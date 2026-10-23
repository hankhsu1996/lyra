# Assertions

Tracks the SystemVerilog assertion family: immediate and deferred immediate assertions (LRM 16.3,
16.4), concurrent assertions and their sequences and properties (LRM 16.5-16.13), sampled value
functions (LRM 16.9.3), the expect statement (LRM 16.17), the assertion control tasks (LRM 20.11),
and checkers (LRM 17).

Each sub-step below says what of its form is checked. Because an assertion observes the design and
never drives it, a run with every one of them removed behaves identically, so `--assertions skip`
elides the whole family rather than rejecting it -- statements, declarations, checkers, and the
control tasks alike. Two members are not removed with it. Sampled value functions, outside an
assertion, produce a value ordinary logic consumes, so there is nothing to elide them to; and an
expect statement is a wait its process stops at (LRM 16.17), so removing one changes when everything
after it runs.

One reporting rule is what a design meets first: an immediate cover's results are reported as one
line per cover statement at the end of a run, on the same channel the design's own output uses --
not as a diagnostic, because a false cover is not a failure and a run over a design whose covers are
only partly reached has to stay one a conforming tool has no comment on. A concurrent cover reaches
no such line yet, and its counters are a different set rather than the same two: attempts, successes
at most one per attempt, and successes owed to vacuity, with the two success counts excluding
disabled evaluations while the attempt count includes them (LRM 16.14.3).

Done when an assertion of each form evaluates its condition at the point the LRM specifies, runs its
pass or fail action in the region the LRM specifies, and reports a failure with its source location;
and when the control tasks act on that state.

## The shape of the problem

Every form shares one condition-evaluation model and differs only in when the resulting action runs.

| Form                     | Condition evaluated    | Matures   | Action runs | LRM   |
| ------------------------ | ---------------------- | --------- | ----------- | ----- |
| Simple immediate         | Inline in process flow | --        | Immediately | 16.3  |
| Observed deferred (`#0`) | Inline in process flow | Observed  | Reactive    | 16.4  |
| Final deferred (`final`) | Inline in process flow | Postponed | Postponed   | 16.4  |
| Concurrent               | Sampled in Observed    | Observed  | Reactive    | 16.5+ |

The three kinds differ in which disposition carries the action: `assert` and `assume` describe a
fail path, `cover` describes a success path. `assume` differs from `assert` in verification intent,
not in simulation behavior.

**The central constraint is that encounter and execution are separated in time.** A deferred action
evaluates its condition and its by-value arguments where it is written, then runs later. By-value
actuals are copied at encounter; `ref` and `const ref` actuals stay bound to their storage and read
its value at execution. A deferred action is restricted to a single subroutine call with no output
or inout arguments, and rejects automatic or dynamic variables as reference actuals -- a rule
stricter than ordinary subroutine passing (LRM 16.4 against 13.5.2). Flushing is per process: when a
process re-triggers, the deferred assertions it enqueued earlier are invalidated, so only the most
recent activation's survive.

That shape -- evaluate now, capture the operands, run in a later region -- is the one non-blocking
assignment already uses. What differs is that an action may wait: the subroutine may be a task, and
a concurrent assertion's action is any procedural statement. So the action is a body of its own,
started in the region it runs in as an execution that belongs to no process's lineage.

### Corner cases worth stating before implementing

These were established once already and are the ones easy to get backwards.

- **A pass action does not replace the default failure report.** All four action combinations behave
  distinctly: with both, the matching one runs; with a pass action only, a false condition still
  emits the default error; with a fail action only, a true condition falls through silently.
- **`assume` mirrors `assert` across every one of those shapes.** It differs in verification intent,
  never in what simulation does.
- **A false cover is not a failure.** It records no hit and must not emit the default assertion
  error, which is what makes the disposition genuinely inverted rather than a renamed assert.
- **Cover hits count per site.** A cover inside a loop accumulates across iterations, and two cover
  statements hold separate counters.
- **Deferred reports are independent per process.** Two processes each holding a pending report
  flush on their own schedule; neither invalidates the other's.
- **The reference-actual restrictions reject three separate things**: an automatic variable, a type
  mismatch against the formal, and a dynamic index. Each is its own diagnostic.

## Sub-Steps

The IDs are stable references and do not imply execution order beyond the dependencies noted.

- [x] AS1 -- Immediate assertions (LRM 16.3). `assert`, `assume`, and `cover` without a timing
      qualifier: the condition is evaluated and the pass or fail action runs in the same procedural
      step. A failure with no user action reports with its source location and severity. This is the
      whole family's condition-evaluation model, so every later item builds on it.
- [x] AS2 -- Deferred immediate assertions (LRM 16.4), both `#0` and `final`. Adds the
      encounter-versus-execution split above: operand capture at encounter, reference actuals bound
      rather than copied, the per-process flush rule, and maturity in Observed (`#0`) or Postponed
      (`final`) with the action running in Reactive or Postponed. Rides on AS1 and on the region
      scheduler. In place on both backends: the expression is evaluated where the statement is
      reached, and the action the outcome selects -- the subroutine call the source wrote, or the
      default report where a false result has none -- is held to Reactive (`#0`) or Postponed
      (`final`), and withdrawn before it matures if the process re-executes, if the assertion's own
      label is disabled, or if the procedure's outermost scope is (LRM 16.4.2, 16.4.4). The call's
      by-value arguments are evaluated where the statement is, on every outcome that selects it,
      whether or not the report is later withdrawn; a `ref` argument is the variable as it is when
      the call runs. The subroutine may be a task, which may wait. One written outside procedural
      code runs as the `always_comb` procedure LRM 16.4.3 makes of it, in a module, an interface and
      a generate block alike, so it is evaluated again whenever what it reads changes and withdrawn
      when that procedure resumes.
- [x] AS3 -- The deferred cover statement (LRM 16.4). The immediate form rides on AS1: the
      disposition is inverted, a cover with no user action records a hit, and the hits are reported
      one line per cover statement at the end of a run. The same statement carrying `#0` or `final`
      holds back both what it records and the statement it carries, so a withdrawn report credits
      nothing and a value met only in passing is not counted.
- [ ] AS4 -- Sampled value functions (`$past`, `$stable`, `$rose`, `$fell`, `$changed`, `$sampled`,
      LRM 16.9.3) used as ordinary logic, outside any assertion. These read a value as of the
      Preponed region, so they need sampling to exist independently of the assertion machinery.
      Inside an assertion they disappear with it, which is why this item is separable from AS5. All
      six are in place on both backends, over every kind of value a variable can hold except an
      opaque handle, whose value is the pointer it carries and which LRM 16.6 keeps out of a
      concurrent assertion anyway, and over every net, whichever way it comes by its value: one
      driver, several resolved, the object its one driver carries, a force, a port with no data
      type. A net's default sampled value is the default of its type. A variable naming an object is
      included: what a tick settles for one is which object it named, and every entry a history
      keeps is a reference to that object, so an object nothing else names is still there for a past
      tick to answer with (LRM 8.4). `$rose` and `$fell` are refused over a value with no least
      significant bit, which is what LRM 16.9.3 defines them on. `$sampled` reads a different value:
      a variable or a net may be armed to answer for the value it held when the current time slot
      began, and an expression's sampled value is that expression over its leaves' sampled ones. The
      other five answer across the ticks of a clocking event, so a scope keeps a history of what is
      read that way, filled with the expression's default sampled value where the design activates
      and appended to by a synthesized process at each tick. That process records in the Postponed
      region of the tick's own time step, which is what makes every tick a reader can see strictly
      prior to it whatever order the two ran in; and because the history starts full, a read
      reaching further back than the ticks that have happened answers with the default sampled value
      without anything counting them. Which event a call counts ticks of follows LRM 16.9.3's
      ordered rules: whichever the source wrote; inside a concurrent assertion's property and its
      action blocks, the assertion's clock; in a procedure, the clock the procedure settles; and
      otherwise the scope's default clocking, a continuous assignment's included. `$past`'s gating
      expression qualifies that event rather than the expression, so two reads of one expression
      under differently gated clocks count different ticks. A constant -- a parameter, one each
      instance is handed its own value of included, a loop generate's index, an enumeration name, a
      package's localparam -- is its own sampled value at every tick. A clocking event that names an
      event ticks at its triggers, as any event control on it does. A clocking declaration is
      carried only as far as naming an event a scope may select as its default; its clockvars,
      skews, drives and the cycle-delay operator are refused.

      An automatic variable, and a `ref` formal that is not `ref static`, answers with its current
      value, and a past value of one is its current value too (LRM 16.5.1), so no cell is armed for
      it and an expression reading only such variables keeps no history.

      Two things are refused rather than answered. A sampled value of a `ref static` formal,
      whose actual's Preponed value is what it should answer with. And a history function over an
      expression reading both an automatic variable and one the design element holds, which needs
      the history kept per variable rather than per expression.

- [ ] AS5 -- Concurrent assertions (LRM 16.5-16.13): sequences, properties, their named
      declarations, `disable iff`, and the clocking a property is evaluated against. Evaluation is
      multi-cycle and against sampled values, which makes this the one form whose semantics are not
      a variation on the immediate model. Rides on AS4.

      In place on both backends for an assertion a scope declares: an attempt begins at every tick
      of the clock, attempts overlap and each carries its own result, and the statements an outcome
      selects run in the Reactive region, as any procedural statement does: one may wait, declare
      variables and disable a named block, and actions that wait run side by side. An attempt
      still in flight when the run ends runs the statement its answer selects before the final
      procedures, up to its first wait. The operator set carried is a Boolean expression, a delay
      window and a consecutive repetition with constant bounds; overlapped and non-overlapped
      implication and followed-by (LRM 16.12.7, 16.12.9); and the properties built from properties
      -- `not`, `and`, `or`, `implies`, `iff`, `if`-`else` and `case` (LRM 16.12.3-16.12.6, 16.12.8,
      16.12.16) -- each nested in any of the others to any depth, in every statement form carried.
      `and` and `or` between two sequences standing as a property are these connectives, which hold
      exactly where the composed sequence would. A named sequence or property taking arguments is
      carried because the front end substitutes the actuals into its body. A sequence standing as a
      property is read at the strength written for it, or otherwise the statement's: strong asks
      that a match exist, weak that no finite prefix has witnessed one cannot (LRM 16.12.2), and
      `not` exchanges the two. Over the operator set carried here the two part company only where
      the trace runs out, so what the strength decides is what an attempt still in flight when the
      run ends comes to: it succeeds where the trace it saw satisfies it, so a weak sequence still
      able to match holds, a strong one does not, and the negation of a weak one fails, as LRM
      16.12.3's own example says.
      `disable iff` preempts every attempt its condition is true during, tested on current values
      across the whole interval rather than at that interval's ticks, so a condition that rises and
      falls between two ticks still preempts.

      What the source leaves unwritten is resolved as LRM 16.15 and 16.16 resolve it. An assertion
      with no clock of its own takes the clock its procedure settles (LRM 16.14.6), or otherwise the
      default clocking, and one whose property or sequence is declared in a clocking block takes
      that block's event; of two clocking events written one against the other, the second is in
      force (LRM 16.13.3). An assertion whose property states no
      disable condition takes the `default disable iff` in force where it stands: its module's,
      interface's or program's, or a generate block's that overrides it, never one from the scope
      the module is instantiated in; `disable iff (1'b0)` cancels it. `$inferred_clock`, a default a property's formal
      may take, names the clock in force where the property is instantiated (LRM 16.14.7), and
      `$inferred_disable` the default disable condition in force where the assertion stands, at
      every depth of nested instances, which is the reading the language conformance rulings record
      for what the clause leaves open; it is carried as a disable condition, which is where it
      stands in every form the clause shows, and refused elsewhere.
      `$global_clock` and the `_gclk` functions are refused: the global clocking is found by
      climbing the instance hierarchy (LRM 14.14), and the future half needs the next tick's value
      (LRM 16.9.4).

      Of the five statement forms the clause admits, three are carried. `restrict` is the fourth and
      is accepted rather than checked, in either placement: it states a constraint for a formal tool
      and is by definition not verified in simulation (LRM 16.14.4), so eliding it is its semantics
      rather than a gap.

      What remains of the form is the other placement: an assertion a procedure reaches and
      simulation checks is refused, because its enabling condition is that control arrived there,
      which is queued rather than evaluated (LRM 16.14.6). Refused by name besides: a local
      variable and a subroutine call attached to a match (LRM 16.10, 16.11), an unbounded `$`
      window, a repetition admitting an empty match, a nonconsecutive or goto repetition, composing
      sequences with `and` / `or` / `intersect` / `throughout` / `within` inside a sequence (an
      antecedent, or an operand of `##`), `first_match`, the operators over time (`nexttime`,
      `always`, `eventually`, `until` and their strong forms), the abort operators, a recursive
      property (LRM 16.12.17), more than one clocking
      event (LRM 16.13), a hierarchical name reaching a variable of a named block in a declared
      assertion's action block (LRM 23.6 allows it, and the clause's example reaches one in an
      immediate assertion's), and the fifth statement form, `cover sequence`, which counts every
      match an attempt produces rather than the attempt.

- [ ] AS6 -- Assertion control tasks (LRM 20.11). They act on assertion state -- turning checking
      on, off, or killing it by scope or by name -- so they need that state to exist first. Two of
      them, `$assertvacuousoff` and `$assertnonvacuouson`, choose by whether a success was vacuous,
      which LRM 16.14.8 defines operator by operator; an attempt carries only its answer today, so
      these need it to carry that as well. Until a control task runs, every pass statement runs on a
      vacuous success too, as 20.11 makes the default.
- [ ] AS7 -- Checkers (LRM 17): checker declarations and instances, which package assertions with
      their own variables and procedural blocks and bind into a design.
- [x] AS8 -- The expect statement (LRM 16.17), on both backends, wherever a wait may stand: in a
      procedure, in a loop, in a task, and in a program. The process reaching it starts one
      evaluation of the property at the next tick of its clock and waits; every occurrence of the
      clock is a tick, two in one time step included, and each is evaluated in the Observed region
      of the time step it happened in, whatever region set the process runs in, over the same
      operator set AS5 carries, with a sequence read strongly (Annex F.3.4.3.1). Once the evaluation
      succeeds or fails the process goes on after that Observed region, in the region its own set
      runs woken work in, and runs the arm the answer selects first; with no else clause a failure
      is the tool's own `$error`. The property reads an automatic variable and a `ref` formal at
      their values in that Observed region (LRM 16.5.1), so a task's `ref` formal sees what the
      nonblocking assignments of that time step wrote, and a program thread's automatic does not yet
      see what that step's Reactive region writes. A `disable iff` condition true at any moment of
      the evaluation, between ticks too, disables it, which is neither a success nor a failure, so
      the process does not go on (LRM 16.12). A process stopped while it waits takes the evaluation
      with it, and `--assertions skip` leaves an expect in place, since its process waits there.
      Refused by name: a clocking event whose expression calls a function or reads through a handle,
      since the wait decides each occurrence where it happens. The front end refuses an `always`
      whose body is an expect as a procedure that cannot advance time.

## Blocked

Nothing is blocked on machinery another workstream owns. The stratified region scheduler is in
place, including the Observed region a deferred check matures in, and the closure-submit path is the
one non-blocking assignment uses.

## Out of Scope

- Formal-verification semantics of `assume`. In simulation it behaves as `assert`; the distinction
  is intent for a formal tool.
- Coverage collection and reporting beyond a cover statement's own hit. Functional coverage
  (`covergroup`, LRM 19) is a separate language surface, tracked in `coverage.md`.

## Cross-references

- LRM anchors: 16.3 (immediate), 16.4 (deferred immediate), 16.5-16.13 (concurrent), 16.9.3 (sampled
  value functions), 17 (checkers), 20.11 (control tasks), 4.4.2.3 (Observed), 4.4.2.4 (Reactive),
  4.4.2.6 (Postponed), 13.5.2 (pass by reference).
- Architecture contracts the work must satisfy: `../architecture/scheduling.md` (which region a
  maturity and an action belong to, and that deferred work is a closure submit rather than a
  suspension), `../architecture/mir.md` (a closure is an expression whose type is its result type).
- Rides on: `processes.md` timing controls and the region machinery a deferred check already uses.
