# Closure

## Purpose

A closure is MIR's one shape for "a body bound to its environment, run later." It is a first-class
callable value with four parts: a list of captured fields, a list of per-invocation parameters, a
result type, and a procedural-scope body. HIR-to-MIR synthesizes closures to model every
SystemVerilog construct that defers a body, runs it concurrently, or runs it once per element -- a
fork-join branch (LRM 9.3.2), a non-blocking assignment, a postponed `$strobe`, a `$sscanf`
write-back, a with-clause iterator (LRM 7.12), a deferred assertion action. No SystemVerilog source
construct is itself a closure; the closure is the lowering's encoding.

A closure is a value: it is built, held, and later invoked. What a closure _is_ does not depend on
who invokes it. How it is invoked -- spawned as a concurrent process, submitted to a scheduling
region, called once per array element -- lives at the referencing site, not on the closure.

## Owns

- The closure value: its captured fields, its parameters, its result type, its body.
- The capture model: how a body reaches the environment it was built in.
- The snapshot-versus-alias distinction for a captured field -- carried by the field's type, not by
  a separate axis.

## Does Not Own

- Whether and how a closure is spawned, submitted, or iterated. That is the referencing site's
  concern; `scheduling.md` owns spawn and submission.
- The suspension protocol of a coroutine body (`scheduling.md`).
- The runtime realization of a reference cell or an observable cell. A reference type and an
  observable type are library wrappers named by MIR types and rendered by a backend; their mechanics
  belong to the runtime, not to the closure.
- The `self` receiver model in general -- shared with every callable form (`mir.md` invariant 11).

## Core Invariants

Each invariant is a consequence of the closure's identity: a value that owns what its body needs,
run later and possibly elsewhere, possibly after the frame that built it is gone.

1. **A closure is synthesized only by HIR-to-MIR.** No SystemVerilog source construct lowers to a
   closure literal; the capture list, parameter list, result type, and body are all
   compiler-generated. _Consequence: a reader never maps a closure back to source syntax; it is
   always the lowering's encoding of a deferred / concurrent / iterated body._

2. **A closure owns everything its body needs; the body reaches the environment only through
   captures and parameters.** A closure value can outlive the frame that built it -- a spawned
   branch runs after its parent returns, a submitted effect runs in a later region. A body that
   reached back into that frame would read storage that may be gone. So the body reads outer state
   only through its own captured fields and its parameters; a `ProceduralVarRef` inside the body
   stays within the body's own scope nesting and never climbs into the enclosing process scope.
   _Consequence: the body is self-contained -- its meaning depends on its captures and parameters,
   not on where it was built._

3. **Whether a captured field is a snapshot or an alias is the field's type, not a separate capture
   kind.** A closure owns its captured fields. A field whose type is a value type `T` is an
   independent snapshot taken at construction: later changes to the source are not seen, and the
   body's writes do not propagate. A field whose type is a reference type aliases the enclosing
   storage cell: reads see the current value and writes propagate (LRM 6.21). There is one capture
   mechanism -- the closure owns a field -- and the field's type alone decides snapshot versus
   alias. _Consequence: there is no "by-value capture" versus "by-reference capture" node kind and
   no flag beside a field naming its mode; the type carries it. A closure is a struct of owned
   fields -- a snapshot field has a value type, an alias field has a reference type._

4. **`self` is the first captured field.** Every callable body reaches its enclosing object's
   members through `self` (`mir.md` invariant 11); a closure carries `self` as `captures[0]`, the
   enclosing scope's own receiver snapshotted at construction. _Consequence: a closure reaches class
   state exactly as any callable does, with no special form._

5. **Coroutine-ness is the result type, not a flag.** A closure whose body suspends (a fork-join
   branch) is a coroutine, and its result type is the coroutine type -- the value its invocation
   yields. A closure whose body runs to completion has a plain result type (a value, or void). The
   decision is made at HIR-to-MIR, where the referencing construct is known, and is carried as the
   closure expression's type. _Consequence: a backend reads "is this a coroutine" from the result
   type, never from a separate flag and never by scanning the body._

## Boundary to Adjacent Layers

- HIR-to-MIR synthesizes closures and fixes their captures, parameters, result type, and body. The
  decisions "this body is a coroutine" and "this field aliases" are made here, where the source
  construct is known, and are recorded in structure -- the result type and the field type
  respectively, never as side flags.
- The referencing site decides invocation: a fork statement spawns each branch closure as a
  concurrent process; a deferred-assignment site submits a closure to a scheduling region; an
  array-method site iterates a closure. The closure does not know which.
- A backend realizes the closure from its captures, parameters, result type, and body alone, by a
  fixed function of each node's kind and type: it reads a field's type to know snapshot versus
  alias, and the result type to know coroutine versus synchronous.
- `scheduling.md` owns spawning, submission, and the suspension protocol; the closure is the unit
  those mechanisms carry.

## Forbidden Shapes

- **A "suspends" / "is-coroutine" flag on the closure node.** Coroutine-ness is already the result
  type (invariant 5); a flag restates a fact the structure fixes, and two facts that must agree
  drift apart under maintenance.

- **A capture-kind axis beside the field type** -- a `by_reference` flag, or distinct
  by-value-capture and by-reference-capture node kinds. Snapshot-versus-alias is the field's type
  (invariant 3); a parallel axis is the redundant-field shape, and it can contradict the type (a
  field typed as a value but flagged an alias). One source: the field type.

- **A body that reaches an enclosing procedural variable by hopping out of the closure's own
  scope.** The frame may be gone when the body runs (invariant 2); outer storage reaches the body
  only as a captured field.

- **A backend that constructs the reference wrapper itself** -- synthesizing `Ref<T>(...)` from a
  flag, or from "this operand looks like an lvalue" -- instead of reading the field's reference
  type. The decision that a field aliases is made at HIR-to-MIR and carried in the field's type; a
  backend that re-derives it is inventing a library shape, which `mir.md` invariant 10 forbids. The
  wrapper appears because the field's type is a reference type and the backend renders that type.

## Notes / Examples

Closures across languages share the same core and differ only at the edges, which is why the model
above is not specific to the C++ backend.

- C++ lambdas capture per variable: `[x]` makes the closure own a copy of `x`; `[&x]` makes it own a
  reference to `x`. Both are owned fields; the field's type (`T` versus `T&`) is the only
  difference. `[&x]` is sugar that hides the reference field.
- Rust closures are structs of owned captured fields; a field's type is `T` (a by-value / move
  capture) or `&T` / `&mut T` (a borrow). `move` forces ownership. By-value versus by-reference is
  the field type.
- Python, JavaScript, and TypeScript closures have one mode: they close over the enclosing
  variable's cell, always a reference to the binding. They have no by-value capture, because a
  garbage-collected language with no manual lifetimes keeps a cell alive for as long as a closure
  refers to it, so there is never a lifetime reason to snapshot.

The by-value/by-reference distinction is a property of value-semantic, lifetime-managed languages
(C++, Rust); garbage-collected languages collapse it to "always reference the cell." SystemVerilog
is value-semantic -- a variable holds a value and assignment copies -- so a captured snapshot (a
value-typed field) is the default, and an alias (a reference-typed field) is the explicit case used
when a body must share mutable storage with its environment: a fork branch writing a variable its
parent reads, a `$sscanf` write-back, the `self` receiver into the shared object.

The C++ backend renders every capture as a by-value lambda capture `[name = <init>]`; it never uses
`[&name]`, `[=]`, or `[this]`. This is a rendering choice with two reasons, not a MIR-level concept.
First, a `[&name]` reference dangles once the closure outlives the captured variable's frame, which
a fork-branch coroutine and a deferred-effect closure routinely do; an owned reference value whose
pointee is long-lived storage (a module signal, a static local) does not. Second, the reference
wrapper routes a write through the cell's update-event path -- writing a signal through it still
wakes the signal's subscribers -- which a bare `T&` would bypass. So an alias field renders as an
owned reference value, not a hidden C++ reference; at the MIR level it is simply a captured field
whose type is a reference type.
