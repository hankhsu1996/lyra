# Event Control Unification

## Date

2026-05-31

## Status

Accepted

## Why this decision matters

SystemVerilog has four constructs whose runtime behaviour is "subscribe to a value-change leaf set,
suspend, wake when a relevant leaf changes":

- `always_comb` / `always_latch` body (LRM 9.2.2.2.1)
- `always @* body` (LRM 9.4.2.2)
- `wait (cond) body` (LRM 9.4.3)
- `@(expr) body`, including event-list forms `@(expr_1 or expr_2)` (LRM 9.4.2)

Before this cut, the first three shared the `SensitivityWaitStmt` machinery introduced for the
continuous-assignment unification (see [read-set-inference.md](read-set-inference.md)). `@(...)` had
a parallel `EventControl` chain whose `EventTrigger.signal` was a single expression and whose
runtime subscription was always at whole-variable granularity. The moment a selector or compound
expression appeared inside `@(...)`, the chain rejected the form with
`kUnsupportedEventTriggerForm`.

The parallel chain was not a small wart. It blocked:

- LRM 9.4.2 "edge event shall be detected only on the LSB of the expression" -- the runtime had no
  per-leaf bit position to monitor, so even simple `@(posedge bus[3])` could not be implemented.
- LRM 9.4.2 "no change in the result of the expression shall not be detected as an event" -- with
  whole-variable subscription, a write to any bit of the variable woke every subscriber, including
  ones whose projection had not changed. Performance regressed and behavioural correctness for
  compound forms was unreachable.
- Reuse of the slang DFA already feeding the other three constructs. The DFA produces a
  `(symbol, bit_range)` leaf set for any expression; `@(...)` ignored that work and looked at the
  source-form structurally instead.

The right shape was to collapse the `@(...)` chain into the `SensitivityWaitStmt` shape, route the
DFA at the event expression, and propagate edge polarity per leaf so the runtime can filter.

## Decision

Every value-change suspension in the simulator goes through one MIR construct,
`mir::SensitivityWaitStmt`, whose `reads` carries per-leaf
`(StructuralVarRef, bit_range, edge_kind)` records. `mir::EventControl` and `mir::EventTrigger` are
deleted.

Delay is a separate, parallel construct: `mir::DelayStmt { duration }` lowers from
`hir::DelayControl` in `LowerTimedStmt`, wrapped in a `BlockStmt` together with the controlled body.
`mir::TimingControl`, `mir::DelayControl`, and `mir::TimedStmt` are deleted -- they were
syntax-grouped artifacts that combined two execution families (clock-anchored delay; data-anchored
value-change) under one variant. MIR is execution-modelling, so the three wait families live at the
same level as disjoint statements:

| Family                | MIR construct                   | Engine subsystem         |
| --------------------- | ------------------------------- | ------------------------ |
| Time-anchored         | `DelayStmt { duration }`        | `Engine::ScheduleAtTime` |
| Value-change anchored | `SensitivityWaitStmt { reads }` | `Observable::Subscribe`  |
| Sync-primitive        | `ExprStmt { event.Await() }`    | `NamedEvent` waiter list |

HIR keeps `hir::TimingControl` as a sum type because HIR is source-faithful and LRM 9.4 groups them
syntactically.

The shape is:

```text
leaves = list of (var, bit_range, edge_kind)
  -- bit_range follows slang's `(lo, hi)` inclusive convention in the var's flat-storage space
  -- edge_kind takes the SV edge identifier on the event expression for single-leaf forms;
     defaults to kAnyChange for implicit sensitivity (always_comb / @* / wait / continuous-assign)
subscribe(every leaf)
suspend
```

The runtime extends `Trigger`, `Observable::Waiter`, and the `Observable::Subscribe` API to carry
`(lsb_bit_offset, bit_width, edge)` per waiter. `Var<PackedArray>::Set` snapshots `var.Get()` before
the assignment, then invokes a per-leaf classifier that compares `old.ExtractBits(lsb, width)`
against `new.ExtractBits(lsb, width)` for any-change waiters and
`ClassifyEdge(old.GetBit(lsb), new.GetBit(lsb))` for edge waiters. The classifier is type-erased via
`std::function`; the `Observable` itself stays value-type agnostic.

At AST -> HIR, `LowerSignalEventTrigger`:

1. Looks up the leaf set in `SensitivityReadStore` keyed by the event expression
   (`SignalEventControl::expr`). The store builder runs a fresh `DefaultDFA` per event expression
   from inside `setCustomDFAProvider`, mirroring the pattern already used for `wait` conds.
2. Attaches the SV edge identifier to each leaf. For the single-leaf case (DFA returned exactly one
   `(symbol, bit_range)` pair), the leaf's bit range is collapsed to `(lo, lo)` so the runtime
   monitors only the LSB of the expression per LRM 9.4.2.
3. Multi-leaf expressions (concatenation, arithmetic, dynamic index, cross-variable) are rejected
   with `kUnsupportedEventTriggerForm` and a "compound event expressions ... are not yet supported"
   message. The deferred implementation is a snapshot + re-eval loop at HIR -> MIR around a
   `SensitivityWaitStmt` whose leaves carry `kAnyChange`. Both halves -- the wrapper and a runtime
   `MatchesEdgeTransition` helper -- are intentionally left to a follow-up so this PR ships a
   bounded slice.

`mir::SensitivityRead.bit_range` retains slang's `(lo, hi)` convention end-to-end. Emit converts to
the runtime's `(lsb, width)` shape at the call site (`width = hi - lo + 1`).

## Consequences

Closed under this PR:

- Every existing `event_*`, `always_comb_*`, `wait_*`, `named_event_*` test continues to pass; the
  named-event path is orthogonal to the unification.
- New tests cover constant bit-select, range-select, indexed part-select (`+:` / `-:`), edge keyword
  combinations, event-list with mixed bit-select members, and `LRM 9.4.2` "no false wake" anchors.

Out of scope but unblocked by this design:

- Compound event expressions: the snapshot + re-eval wrapper is the natural next step. The HIR
  carries `EventTrigger.sensitivity_list` for every event expression, so the wrapper just needs to
  iterate triggers, materialise per-trigger snapshot vars, and call the deferred edge helper.
- Ascending / negative base packed ranges, multi-dim packed events, struct / unpacked event
  triggers. Each is bounded by a separate workstream (PackedArray direction handling, multi-dim
  initializer emit, type infrastructure).

## Alternatives considered

**Keep `EventControl` and add per-leaf metadata in parallel.** Rejected: the parallel chain
duplicates the wait-suspension semantics already lived through `SensitivityWaitStmt` plus the slang
DFA. Two shapes for one runtime behaviour is exactly the kind of seam we keep paying down.

**Move LRM 9.4.2 LSB-reduce to runtime.** Rejected: the LRM rule is "edge events detect only the LSB
of the expression". For a single-leaf event whose expression is `bus[hi:lo]` the LSB is statically
the leaf's `lo`. Computing that at codegen time produces a runtime subscription with `width = 1`,
which is both correct and cheap. Pushing it to runtime would require the runtime to know the
expression's LSB semantics, which is a frontend concern.

**Always emit a snapshot + re-eval loop even for simple single-leaf forms.** Rejected: per-leaf
runtime filtering already produces LRM-correct wake decisions when the expression is a single leaf
(its projection covers the full result). The snapshot loop is overhead in that case; the wrapper is
only necessary when leaf changes don't entail an expression-level change. The HIR side does the
classification cheaply by looking at `sensitivity_list.size()`.

**Keep `mir::TimingControl` as a single-arm variant of `DelayControl`.** Rejected: a variant with
one arm carries no polymorphism, only the historical shape of an abstraction that no longer applies.
The LRM groups `#N`, `@(...)`, and `wait` syntactically because they share the
procedural-timing-control grammar position; the engine treats them as three disjoint subsystems
(time queue vs Observable subscription vs NamedEvent waiter list). MIR is execution-modelling, so it
follows the execution split, not the syntactic one.
