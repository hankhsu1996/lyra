# Object model: managed handle, its reclamation, and the instance-method receiver

Date: 2026-06-25 (revised 2026-06-27) Status: accepted

## Why this decision matters

The object model -- a module instance, a generate scope, and a SystemVerilog class as one generic
nominal object type -- is a contract in `../architecture/object_model.md`. Most of that model is
stated there as invariants and forbidden shapes and needs no trade-off record: the shape is the
shape. Two choices inside it have a plausible alternative that a future reader will re-propose, and
the contract deliberately states only the rule, not the rejected option. This entry records the two,
so the rule is not relaxed back into the alternative it was chosen over. The rest of the design
journey (the two-IR split we declined, an early parallel reference-type hierarchy we drafted and
withdrew) left no load-bearing residue and is not recorded here.

## Decision 1: a SystemVerilog class handle is a managed reference, not the shared one

The references that reach an object instance are owning, borrowed, shared, and managed. A
SystemVerilog class handle (LRM 8.3) is the **managed** kind: null is a legal value, identity is
comparable, copies are shallow, the object is reachable while any handle reaches it, it is created
by `new`, and it is never explicitly freed.

The tempting alternative is to reuse the existing **shared** reference: it is already present and
already lowers to a reference-counted pointer, so an SV handle could ride it with no new kind. It is
rejected because the shared reference is **already committed**, by
[`lifetime-extended-automatic-scope`](lifetime-extended-automatic-scope.md), to a reference-counted
DAG that is **acyclic by construction** (an activation owns only data slots, never a process, a
closure, or a join group, so no ownership cycle can form). SystemVerilog class objects routinely
form reference cycles -- two handles pointing at each other is ordinary LRM 8 code. Placing cyclic
object graphs under a reference whose contract promises acyclicity would break that prior decision's
invariant. The managed kind carries the reachability semantics SystemVerilog classes need; how that
reachability is realized is Decision 3.

A second alternative -- a separate parallel object-reference type carrying its own kind and
nullability axes -- is also rejected: it would fold the deliberately distinct reference type (the
observable-cell alias of [`reference-as-data-type`](reference-as-data-type.md)), the plain pointer,
and the observable storage wrapper into one classifier and lose the observable-cell-protocol
distinction that decision established. The managed kind rides the existing one-level-pointer
ownership axis instead; nullability stays a value-level fact (a null literal), not a type axis,
until an analysis that reads it exists.

## Decision 2: the receiver is a property of an instance method, not of every callable

The receiver `self` is an **instance method's** first parameter. A type-associated (static) function
(LRM 8.10) -- a method called on the type, with no object -- has no receiver and no `self`.

This scopes a prior wording. The callable contract stated that _every_ callable body's first binding
is `self`; with static methods that becomes false, because a static method has no instance to
receive. The alternative -- give a static method a fabricated, unused `self` -- is rejected: it
encodes "there is no instance" as "there is an instance nobody uses," which is semantically false
and forces every backend and every call site to carry and supply a receiver a static call does not
have. `mir.md` invariant 11 and `callable.md` invariant 2 are scoped accordingly: an instance method
has `self`, a static function does not.

## Decision 3: the managed reference is realized by precise tracing garbage collection

A managed object's lifetime is reachability: it is retained while reachable from runtime roots
through managed edges, and reclaimed by a precise tracing collector at a runtime safepoint. Three
alternatives were weighed and rejected.

A **simulation-lifetime arena** -- allocate, never reclaim until shutdown -- is the simplest
realization and was the early lean. It is rejected as the model because memory grows without bound
under allocation-heavy testbenches (a `forever` loop allocating a transaction per cycle),
conflicting with the bounded-memory requirement. It is a valid intermediate state during
implementation, never a terminal one for class support.

**Pure reference counting** reclaims acyclic garbage promptly and needs no root enumeration, but
SystemVerilog class graphs routinely form cycles (`a.next = b; b.next = a; a = null; b = null`
leaves two mutually-referencing objects no handle reaches). Reference counting cannot reclaim an
unreachable cycle, so it leaks legally-created object graphs.

**Reference counting plus a cycle collector** is semantically sufficient but inherits the hard part
of tracing -- root discovery, graph traversal, safepoints, enumerating managed references in
suspended execution state -- while still paying reference-count traffic on every handle copy and
running two lifetime systems. Absent profiling that shows reference counting materially helps the
hot path, it is not the base model.

Precise tracing requires that every managed reference live at a safepoint be enumerable. A backend's
coroutine machinery places cross-suspend locals in an opaque frame with no enumeration interface, so
tracing is impossible while managed state hides there. The enabling decision is therefore
structural: every language-visible value that can survive a safepoint lives in compiler-described,
Lyra-owned, traceable storage -- the activation frame and traceable closures -- reached through
Lyra-visible runtime records, never through opaque backend execution state. An activation frame is
owned by its execution record (it is not itself a managed object) and exposes a trace operation; the
managed edges inside it are what the collector follows. This storage discipline, the root
categories, and the safepoint contract are owned by `../architecture/object_lifetime.md`; reclaiming
cyclic garbage is part of the model, not a gap.

## Consequences

- `../architecture/object_model.md` states the managed reference (invariants 4 and 9) and the
  instance-method receiver rule (invariant 7) as contract; this entry holds the rejected
  alternatives behind them.
- `mir.md` invariant 11 and `callable.md` invariant 2 are scoped to instance methods; a static
  function is the no-receiver callable.
- The managed reference is the SystemVerilog class handle's reference kind, distinct from the shared
  (acyclic, reference-counted) one. Its lifetime is realized by precise tracing garbage collection,
  and cyclic object graphs are reclaimed by reachability. The lifetime contract and the storage
  discipline that makes tracing precise are owned by `../architecture/object_lifetime.md`.
- A backend may realize the managed reference through shared ownership as an implementation staging
  mechanism while other pipeline surfaces mature. This staging does not satisfy the terminal
  managed-lifetime requirement -- cyclic reclamation remains precise tracing's job -- and does not
  enter the semantic model any consumer reads.

## Cross-references

- `../architecture/object_model.md` -- the object-model contract these choices sit inside.
- `../architecture/object_lifetime.md` -- the managed-object lifetime contract Decision 3
  establishes: reachability, precise tracing, activation frames, safepoints, and roots.
- `lifetime-extended-automatic-scope.md` -- the shared reference's acyclic-DAG contract that
  Decision 1 declines to violate.
- `reference-as-data-type.md` -- the observable-cell reference type Decision 1 declines to fold
  away.
- `../architecture/callable.md`, `../architecture/mir.md` -- the receiver invariants Decision 2
  scopes.
