# Assertions

Tracks the SystemVerilog assertion family: immediate and deferred immediate assertions (LRM 16.3,
16.4), concurrent assertions and their sequences and properties (LRM 16.5-16.13), sampled value
functions (LRM 16.9.3), the assertion control tasks (LRM 20.12), and checkers (LRM 17).

Nothing in the family lowers today. Because an assertion observes the design and never drives it, a
run with every one of them removed behaves identically, so `--disable-assertions` elides the whole
family rather than rejecting it -- statements, declarations, checkers, and the control tasks alike.
Sampled value functions are the one member that cannot cover: outside an assertion they produce a
value ordinary logic consumes, so there is nothing to elide them to.

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
assignment already uses. The deferred action is a closure submitted to a region, not a mechanism of
its own.

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

- [ ] AS1 -- Immediate assertions (LRM 16.3). `assert`, `assume`, and `cover` without a timing
      qualifier: the condition is evaluated and the pass or fail action runs in the same procedural
      step. A failure with no user action reports with its source location and severity. This is the
      whole family's condition-evaluation model, so every later item builds on it.
- [ ] AS2 -- Deferred immediate assertions (LRM 16.4), both `#0` and `final`. Adds the
      encounter-versus-execution split above: operand capture at encounter, reference actuals bound
      rather than copied, the per-process flush rule, and maturity in Observed (`#0`) or Postponed
      (`final`) with the action running in Reactive or Postponed. Rides on AS1 and on the region
      scheduler.
- [ ] AS3 -- Cover statements (LRM 16.7). Inverts the disposition: the interesting path is the
      success one, and a cover with no user action records a hit. How hits are surfaced is a tool
      feature rather than a language one, so the count's reporting form is open.
- [ ] AS4 -- Sampled value functions (`$past`, `$stable`, `$rose`, `$fell`, `$changed`, `$sampled`,
      LRM 16.9.3) used as ordinary logic, outside any assertion. These read a value as of the
      Preponed region, so they need sampling to exist independently of the assertion machinery.
      Inside an assertion they disappear with it, which is why this item is separable from AS5.
- [ ] AS5 -- Concurrent assertions (LRM 16.5-16.13): sequences, properties, their named
      declarations, `disable iff`, and the clocking a property is evaluated against. Evaluation is
      multi-cycle and against sampled values, which makes this the one form whose semantics are not
      a variation on the immediate model. Rides on AS4.
- [ ] AS6 -- Assertion control tasks (LRM 20.12). They act on assertion state -- turning checking
      on, off, or killing it by scope or by name -- so they need that state to exist first.
- [ ] AS7 -- Checkers (LRM 17): checker declarations and instances, which package assertions with
      their own variables and procedural blocks and bind into a design.

## Blocked

Nothing is blocked on machinery another workstream owns. The stratified region scheduler is in
place, including the Observed region a deferred check matures in, and the closure-submit path is the
one non-blocking assignment uses.

## Out of Scope

- Formal-verification semantics of `assume`. In simulation it behaves as `assert`; the distinction
  is intent for a formal tool.
- Coverage collection and reporting beyond a cover statement's own hit. Functional coverage
  (`covergroup`, LRM 19) is a separate language surface.

## Cross-references

- LRM anchors: 16.3 (immediate), 16.4 (deferred immediate), 16.5-16.13 (concurrent), 16.9.3 (sampled
  value functions), 17 (checkers), 20.12 (control tasks), 4.4.2.3 (Observed), 4.4.2.4 (Reactive),
  4.4.2.6 (Postponed), 13.5.2 (pass by reference).
- Architecture contracts the work must satisfy: `../architecture/scheduling.md` (which region a
  maturity and an action belong to, and that deferred work is a closure submit rather than a
  suspension), `../architecture/mir.md` (a closure is an expression whose type is its result type).
- Rides on: `processes.md` timing controls and the region machinery a deferred check already uses.
