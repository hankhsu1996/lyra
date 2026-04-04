# Assertions

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. A1a). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items. Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for SystemVerilog assertion support. The goal is staged assertion enablement, starting from the highest-impact subset and expanding toward broader language coverage.

Scope: `assert`, `assume`, `cover property`, `cover sequence`, `expect` are simulation-relevant. `restrict` has no simulation semantics (LRM 16.14.4) and is out of scope -- skip silently.

## Progress

- [ ] A1 -- Immediate assertion statements
  - [x] A1a -- Basic immediate assert: pass/fail behavior
  - [x] A1b -- Immediate assume and immediate cover
  - [x] A1c -- Immediate assertion reporting: default failure actions and severity

- [ ] A2 -- Deferred immediate assertions
  - [ ] A2a -- Deferred `#0` evaluation path
  - [ ] A2b -- Per-process deferred assertion report queue and flush rules
  - [ ] A2c -- Deferred `final` assertion path
  - [ ] A2d -- Deferred-final flush hook (after final blocks complete)

- [ ] A3 -- Assertion scheduling and sampled-state infrastructure
  - [ ] A3a -- Preponed sampled-value capture boundary
  - [ ] A3b -- Observed-region concurrent assertion evaluation
  - [ ] A3c -- Reactive-region scheduler support
  - [ ] A3d -- Deferred assertion queue flush on Reactive
  - [ ] A3e -- Concurrent cover pass-action execution on Reactive

- [ ] A4 -- Sampled value functions
  - [ ] A4a -- One-step sampled comparisons (`$rose`, `$fell`, `$stable`, `$changed`)
  - [ ] A4b -- Sampled history with explicit depth (`$past`)
  - [ ] A4c -- `$sampled` (trivial once A3a exists)

- [ ] A5 -- Concurrent assertion core
  - [ ] A5a -- Property and sequence IR representation
  - [ ] A5b -- Boolean property forms (simple expr, not, iff, conditional, case)
  - [ ] A5c -- Implication operators (`|->`, `|=>`, `#-#`, `#=#`)
  - [ ] A5d -- Abort and disable conditions (`disable iff`, `accept_on`, `reject_on`)
  - [ ] A5e -- Basic sequence delay concatenation (`##N`, `##[M:N]`)
  - [ ] A5f -- Sequence boolean composition (and, or)
  - [ ] A5g -- Alignment-sensitive sequence operators (intersect, throughout, within, first_match)
  - [ ] A5h -- Consecutive repetition (`[*N]`, `[*M:N]`)
  - [ ] A5i -- Goto and non-consecutive repetition (`[->]`, `[=]`)
  - [ ] A5j -- Temporal property operators (nexttime, always, eventually)
  - [ ] A5k -- Until family and strong/weak qualifiers

- [ ] A6 -- Concurrent cover
  - [ ] A6a -- Cover property (at-most-once success counting per evaluation)
  - [ ] A6b -- Cover sequence (match multiplicity counting per attempt)

- [ ] A7 -- Expect statements

- [ ] A8 -- Assertion control tasks

- [ ] A9 -- Assertion usage profiling and staged enablement

## A1b: Immediate assume and immediate cover

Procedural `assume(expr)` and `cover(expr)`. Assume behaves like assert in simulation (LRM 16.3). Immediate cover has `statement_or_null` (no else clause) -- semantics are "record that condition was observed," not "check that it holds." Shares lowering surface with A1a but the reporting and action-block shapes differ.

## A2a: Deferred `#0` evaluation path

`assert #0 (expr)` defers check to later in the time step rather than evaluating inline. The condition is captured at the point of execution; the report is deferred. This adds scheduling-region semantics beyond ordinary procedural execution.

## A2b: Per-process deferred assertion report queue and flush rules

Deferred assertions maintain a per-process pending report queue (LRM 16.4.2). This is distinct runtime infrastructure from just having a Reactive region. Flush points: process resumes from event control/wait, always_comb/always_latch re-triggers, scope disabled, process ends. No coalescing -- every evaluation is independent.

## A2c: Deferred `final` assertion path

`assert final (expr)` defers check to end-of-simulation. Different runtime trigger point from `#0` deferred assertions. Depends on `final` block support (tracked in language queue as L6).

## A2d: Deferred-final flush hook

The deferred-final report queue flushes after all `final` blocks have completed. This is a separate hook from `final` block execution itself -- `assert final` reports come after final blocks run, not during.

## A3a: Preponed sampled-value capture boundary

Sampled-value functions and concurrent assertions depend on a stable snapshot of signal values captured before any Active-region updates execute. This is the Preponed-region semantic from LRM 4.4. Pure infrastructure -- provides the sampling foundation for A3b, A4, and A5.

## A3b: Observed-region concurrent assertion evaluation

The Observed region is where concurrent assertion property evaluation executes, after Active/Inactive/NBA regions have converged. Together with A3a, forms the assertion evaluation substrate.

## A3c: Reactive-region scheduler support

Add the Reactive region to the scheduler. Pure scheduling infrastructure -- the region exists and can execute queued work. Consumers (A3d, A3e) are separate items.

## A3d: Deferred assertion queue flush on Reactive

Wire deferred `#0` assertion report queue flush into the Reactive region. Depends on A2b (report queue) and A3c (Reactive region). This is the integration point, not the infrastructure itself.

## A3e: Concurrent cover pass-action execution on Reactive

Concurrent cover pass actions execute in the Reactive region. Depends on A3c (Reactive region) and A6 (concurrent cover semantics). Separate from deferred flush because the trigger and execution model differ.

## A4a: One-step sampled comparisons

`$rose(expr)`, `$fell(expr)`, `$stable(expr)`, `$changed(expr)`. Compare current sampled value with immediately previous sampled value. Smallest useful sampled-value slice. Depends on A3a for the capture boundary.

## A4b: Sampled history with explicit depth

`$past(expr, n)` with explicit depth. Needs a per-expression N-deep history model (ring buffer or equivalent). Separate from A4a because the runtime state shape is a different magnitude.

## A4c: `$sampled`

`$sampled(expr)` returns the Preponed-captured value. Trivial once A3a exists.

## A5a: Property and sequence IR representation

HIR/MIR representation for property expressions, sequence expressions, and clocking blocks. This is the frontend/IR layer -- does not include the runtime evaluation engine. Slang already provides a rich `AssertionExpr` tree; this item is about projecting it into Lyra's IR. Scope must be held strictly to representation, not evaluation.

## A5b: Boolean property forms

Simplest concurrent assertion: `assert property (@(posedge clk) expr)` where expr is a single-step boolean. Also includes property-level `not`, `iff`, conditional (`if-else`), and `case` property selection. No temporal state needed -- evaluation is stateless per clock edge.

## A5c: Implication operators

`|->` (overlapping implication), `|=>` (non-overlapping implication), `#-#` (overlapped followed-by), `#=#` (non-overlapped followed-by). These share the same evaluation model: antecedent match triggers consequent evaluation, with overlapping vs one-tick-delayed start.

## A5d: Abort and disable conditions

`disable iff (expr)`, `accept_on(expr)`, `reject_on(expr)`, `sync_accept_on(expr)`, `sync_reject_on(expr)`. These are property wrappers that conditionally abort evaluation. Separate from implication because they modify the evaluation lifecycle rather than composing antecedent/consequent.

## A5e: Basic sequence delay concatenation

`##N` (fixed delay), `##[M:N]` (range delay). The fundamental sequence composition operator. This is where the evaluator first needs multi-cycle state tracking -- a pending match that matures after N cycles.

## A5f: Sequence boolean composition

`and`, `or` on sequences. Both sub-sequences evaluate concurrently; `and` requires both to match (same start), `or` requires either. Simpler than alignment-sensitive operators because no length constraint is imposed.

## A5g: Alignment-sensitive sequence operators

`intersect` (both match, same length), `throughout` (expression holds during entire sequence), `within` (sequence completes within another), `first_match` (restrict to earliest match). These impose constraints on match alignment or duration, adding complexity beyond simple composition.

## A5h: Consecutive repetition

`[*N]`, `[*M:N]`. Sequence must match on consecutive cycles for the specified count or range. The most common repetition form. Requires cycle-counting state in the evaluator.

## A5i: Goto and non-consecutive repetition

`[->N]` / `[->M:N]` (goto -- ends at last iterative match), `[=N]` / `[=M:N]` (non-consecutive -- does not end at last match). Distinct match-end semantics from consecutive repetition. Less common but semantically different enough to warrant a separate cut.

## A5j: Temporal property operators

`nexttime` / `s_nexttime`, `always` / `s_always`, `eventually` / `s_eventually`. These are property-level temporal operators (LTL-like). `nexttime` is bounded (one tick); `always` and `eventually` can be bounded or unbounded. Separate subsystem from sequence-level repetition.

## A5k: Until family and strong/weak qualifiers

`until` / `s_until` / `until_with` / `s_until_with`. Binary temporal operators: property P1 holds until P2 becomes true. `strong` / `weak` qualifiers on sequences and properties control liveness vs safety semantics. Grouped because both deal with open-ended temporal commitment.

## A6a: Cover property

`cover property (prop_spec) stmt_or_null`. At-most-once success counting per evaluation attempt. Pass action executes in Reactive region. Different counting model from cover sequence.

## A6b: Cover sequence

`cover sequence (seq_expr) stmt_or_null`. Match multiplicity counting -- total matches per attempt, including multiple matches from the same attempt. Pass action executes in Reactive region per match.

## A7: Expect statements

`expect (property_spec) action_block`. Procedural blocking statement that suspends until property resolves (LRM 16.17). Like a single-shot concurrent assertion that blocks the caller. Depends on concurrent assertion engine (A5b+). Rare in RTL.

## A8: Assertion control tasks

`$asserton`, `$assertoff`, `$assertkill`, `$assertcontrol`. Runtime enable/disable of assertion checking. Control type flags include assertion-type bitmask (concurrent, simple immediate, observed deferred, final deferred, expect, unique, unique0, priority). The unique/priority flags tie back to decision violation control.

## A9: Assertion usage profiling and staged enablement

Track assertion support against real workload usage patterns, not only language taxonomy. Classify currently unsupported assertions by feature subset and use that profile to drive staging order for enablement. Practical progress depends on unlocking high-usage assertion forms first.
