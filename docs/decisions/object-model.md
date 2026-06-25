# Object model: managed handle, and the receiver is an instance-method property

Date: 2026-06-25 Status: accepted

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
invariant. The managed kind carries the reachability semantics SystemVerilog classes need; whether a
backend realizes it by reference counting (the current realization) or tracing collection (a future
one) is a realization choice, and reclaiming cyclic garbage is not part of the contract.

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

## Consequences

- `../architecture/object_model.md` states the managed reference (invariants 4 and 9) and the
  instance-method receiver rule (invariant 7) as contract; this entry holds the rejected
  alternatives behind them.
- `mir.md` invariant 11 and `callable.md` invariant 2 are scoped to instance methods; a static
  function is the no-receiver callable.
- The managed reference is the SystemVerilog class handle's reference kind, distinct from the shared
  (acyclic, reference-counted) one; cyclic class garbage is not yet reclaimed, and that is a
  realization gap, not a semantic commitment.

## Cross-references

- `../architecture/object_model.md` -- the object-model contract these two choices sit inside.
- `lifetime-extended-automatic-scope.md` -- the shared reference's acyclic-DAG contract that
  Decision 1 declines to violate.
- `reference-as-data-type.md` -- the observable-cell reference type Decision 1 declines to fold
  away.
- `../architecture/callable.md`, `../architecture/mir.md` -- the receiver invariants Decision 2
  scopes.
