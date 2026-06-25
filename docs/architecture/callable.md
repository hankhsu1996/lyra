# Callable

## Purpose

A callable is MIR's one shape for "a body with a signature, run when invoked." There is a single
callable concept; the apparent variety of SystemVerilog forms -- function, task, process,
constructor, and the compiler-synthesized closures (a fork-join branch, a non-blocking assignment, a
postponed `$strobe`, a `$sscanf` write-back, a with-clause iterator, a deferred assertion action) --
are all this one concept, distinguished only by their signature, how they bind their environment,
and how a referencing site uses them. None is a distinct kind of callable.

The concept has two structural levels:

- **Callable code** -- a declaration: a signature plus an implementation form, which is either an
  internal body or an external symbol.
- **Callable value** -- callable code plus a bound environment: the runtime value built, held, and
  later invoked. A closure is a callable value with a captured environment; a registered process is
  a callable value that binds its instance receiver.

This is the conceptual peer of how Rust, C++, and Python treat callables: a function item is code, a
closure is code plus a captured environment, and "async" is the result type (`async fn f() -> T` is
`fn f() -> impl Future<Output = T>`), never a separate kind of function.

## Owns

- The callable concept: callable code (signature plus implementation form) and callable value (code
  plus a bound environment).
- The signature: a parameter list (each parameter a typed binding) and a result type that carries
  the call protocol.
- The environment model: how a callable value binds the state its body needs, and the
  snapshot-versus-alias distinction for a bound field, carried by the field's type.
- The implementation-form distinction: an internal body versus an external (foreign) symbol.

## Does Not Own

- How a referencing site invokes or registers a callable -- a direct call, a fork spawn, a region
  submit, a per-element iteration, a constructor-time lifecycle registration. That is the
  referencing site's concern; `scheduling.md` owns spawn and submission.
- The suspension protocol of a coroutine body (`scheduling.md`).
- Object and class layout, dispatch tables, and inheritance. How a call resolves its target --
  directly or by dynamic dispatch -- is a property of the call site and the object's type layout,
  owned by the object model, not of the callable body.
- The runtime realization of a reference cell or an observable cell, and storage placement --
  runtime and LIR concerns.

## Core Invariants

1. **A callable is code; a callable value is code plus a bound environment.** A direct call
   references code and supplies every parameter per invocation, constructing no value. A callable
   value materializes only where a callable is deferred, stored, forked, submitted, or otherwise
   invoked later by a site that does not supply the environment. _Consequence: captures belong to a
   callable value's construction, not to a code declaration; one body can take different bound
   environments._

2. **`self` is an instance method's first parameter.** Every instance-method body reaches its
   enclosing object's members through `self`, the code's first ordinary parameter, read as
   `body.vars[0]` by every member access. A callable value binds `self` into its environment; a
   direct call supplies it per invocation. Whether a bound `self` is realized as a passed parameter
   or a captured field is a backend realization driven by lifetime, not a MIR distinction. A
   type-associated (static) function has no receiver and no `self`. _Consequence: there is no
   privileged `self` capture slot and no per-form receiver mechanism; `self` is uniform across
   instance methods, and a static function simply omits it._

3. **The result type is the call protocol.** A coroutine result type means the callable is entered
   and completed through the coroutine protocol -- the invocation yields a coroutine object the site
   must `await` or `spawn`. The result type is parameterized: `Coroutine<T>` carries the value the
   completion yields. _Consequence: "function versus task" and a process's coroutine-ness are the
   result type, never a kind tag. "The body can suspend" is a separate body-analysis fact, not the
   result type's meaning -- a `final` block never suspends yet is a coroutine through the protocol._

4. **Parameter direction is data flow.** An `input` is a value parameter; a `ref` / `const ref` is a
   reference-typed parameter (a live alias, LRM 6.21); an `output` is a component of the result's
   output pack; an `inout` is a value parameter plus an output-pack component. _Consequence: there
   is no direction enum and no callable-level direction field. `output` / `inout` copy out at
   completion because their values ride the result, written by the call site after completion; only
   `ref` is a live alias._

5. **A bound field is a snapshot or an alias by its type.** A field whose type is a value type is an
   independent snapshot taken at construction; a field whose type is a reference type aliases the
   enclosing storage cell (reads see the current value, writes propagate, LRM 6.21). There is one
   binding mechanism -- the value owns a field -- and the field's type alone decides snapshot versus
   alias. _Consequence: there is no by-value-versus-by-reference capture axis beside the field
   type._

6. **A callable value's body is self-contained.** A callable value can outlive the frame that built
   it -- a spawned branch runs after its parent returns, a submitted effect runs in a later region.
   The body reaches outer state only through its bound environment and its parameters, never by
   climbing out of its own scope. _Consequence: a value reference inside the body stays within the
   body's own scope nesting; outer storage reaches the body only as a bound field._

7. **The implementation form is internal or external.** An internal callable has a body. An external
   callable -- a DPI import -- is a foreign-symbol declaration: a signature plus a linkage name, a
   foreign language, and a calling convention, with no body. This symbol contract is explicit
   structure, read identically by every backend; only the mechanical value marshaling is backend
   realization. _Consequence: a bodyless callable is a structural variant, mirroring how an object
   is intra-unit or external-unit -- not a flag on an internal callable._

## Boundary to Adjacent Layers

- HIR-to-MIR synthesizes callable code and callable values, fixing each signature, each result type
  (the call protocol), and each bound field's type (snapshot versus alias). The compiler-synthesized
  closures are the lowering's encoding of a deferred, concurrent, or per-element body; no
  SystemVerilog source construct is itself a closure.
- The referencing site decides use: a direct call, a fork that spawns each branch value, a region
  submit, a per-element iteration, or a constructor-time lifecycle registration (a process is an
  instance-bound callable value registered during construction; a finalization hook is a distinct
  registration from a startup one). The callable does not know which.
- A backend realizes a callable from its signature, its environment, and its body by a fixed
  function of each node's kind and type: the snapshot-versus-alias of each bound field from its
  type, the call protocol from the result type. The param-versus-capture realization of `self` and
  the marshaling of an external callable are backend choices.

## Forbidden Shapes

- **A kind tag on a callable body** -- a function/task discriminator, a process kind, an async/sync
  flag. Function-versus-task and process-coroutine-ness are the result type; a tag restates a fact
  the structure fixes, and two facts that must agree drift apart.

- **A "suspends" / "is-coroutine" flag.** Coroutine-ness is the result type (invariant 3).

- **`self` as a privileged capture slot or a per-form receiver mechanism.** `self` is the code's
  receiver parameter (invariant 2); a callable value binds it as one ordinary environment field.

- **A parameter-direction enum or a callable-level direction field.** Direction is data flow in the
  signature (invariant 4).

- **A capture-kind axis beside the field type** -- a `by_reference` flag, or distinct
  by-value-capture and by-reference-capture node kinds. Snapshot-versus-alias is the field's type
  (invariant 5); a parallel axis can contradict the type.

- **A body that reaches an enclosing variable by hopping out of its own scope.** The frame may be
  gone when the body runs (invariant 6); outer storage reaches the body only as a bound field.

- **A backend that constructs the reference wrapper itself** -- synthesizing a reference value from
  a flag or from "this operand looks like an lvalue" -- instead of reading the bound field's
  reference type. The decision that a field aliases is made at HIR-to-MIR and carried in the field's
  type; a backend that re-derives it is inventing a library shape (`mir.md` invariant 10).

- **A lifecycle registration as a static class-level list.** A process registration can depend on
  generate conditions and generate-for multiplicity, so it is per-instance, constructor-time work; a
  class holds at most a registration descriptor.

## Notes / Examples

A parameter's direction lowers entirely into the signature's data flow. A task

```text
task t(input int a, output int b, inout int c);
```

has the normalized contract `t : (self, int a, int c_in) -> Coroutine<Outputs{ b: int, c: int }>`.
The call site supplies `a` and the snapshot of `c`, and after completion writes each output-pack
component to the caller's actual place. Copy-out happens after completion because the values ride
the result; an `output` formal has no copy-in (it is a fresh body local); only a `ref` parameter is
a live alias.

Closures across languages share the same core and differ only at the edges. C++ lambdas capture per
variable: `[x]` owns a copy of `x`, `[&x]` owns a reference to `x`; both are owned fields, and the
field's type (`T` versus `T&`) is the only difference. Rust closures are structs of owned captured
fields, a field's type being `T` (by value) or `&T` / `&mut T` (a borrow). Python, JavaScript, and
TypeScript have one mode -- they close over the enclosing cell -- because a garbage-collected
language keeps a cell alive as long as a closure refers to it, so there is never a lifetime reason
to snapshot. SystemVerilog is value-semantic, so a captured snapshot (a value-typed field) is the
default and an alias (a reference-typed field) is the explicit case used when a body must share
mutable storage with its environment.

The C++ backend renders every capture as a by-value lambda capture (`[name = <init>]`); it never
uses `[&name]`, `[=]`, or `[this]`. A reference-typed field renders as an owned reference value, not
a hidden C++ reference: a `[&name]` reference dangles once the value outlives the captured
variable's frame -- which a fork-branch coroutine and a deferred-effect closure routinely do -- and
the reference wrapper routes a write through the cell's update-event path, which a bare `T&` would
bypass.
