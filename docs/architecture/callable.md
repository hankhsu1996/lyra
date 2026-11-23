# Callable

## Purpose

A callable is MIR's one shape for "a body with a signature, run when invoked." There is a single
callable concept; the apparent variety of SystemVerilog forms -- function, task, process,
constructor, a DPI-C import or export, and the compiler-synthesized closures (a fork-join branch, a
non-blocking assignment, a postponed `$strobe`, an event control's watch, a deferred assertion
action) -- are all this one concept, distinguished only by their signature, how they bind their
environment, who declares them, and how a referencing site uses them. None is a distinct kind of
callable.

The concept has two structural levels:

- **Callable code** -- a declaration: a signature, plus a body where this declaration also defines
  the callable.
- **Callable value** -- callable code plus a bound environment: the runtime value built, held, and
  later invoked. A closure is a callable value with a captured environment; a registered process is
  a callable value that binds its instance receiver.

This is the conceptual peer of how Rust, C++, and Python treat callables: a function item is code, a
closure is code plus a captured environment, and "async" is the result type (`async fn f() -> T` is
`fn f() -> impl Future<Output = T>`), never a separate kind of function.

Every fact that looks like it belongs to a callable has exactly one of four homes: the signature
(parameter types and the result type), the environment (what a callable value binds), the
referencing site (call and await, fork, submit, construct, dispatch, register), or a backend's
realization (calling convention, marshaling, parameter versus capture). Only the first two are
structure on the callable; a fact of the last two written on a callable body is a kind tag that has
merely moved.

## Owns

- The callable concept: callable code (a signature, and a body where defined) and callable value
  (code plus a bound environment).
- The signature: a parameter list (each parameter a typed binding), the receiver binding where the
  body is entered on an object, and a result type that carries the call protocol and the completion
  payload.
- Who declares a callable -- the unit's namespace, a class, a class's construction, a closure, a
  record -- and the callable's identity as its entry in the unit's one table of callables.
- The environment model: how a callable value binds the state its body needs, and the
  snapshot-versus-alias distinction for a bound field, carried by the field's type.
- The declaration-versus-definition distinction, the foreign-linkage axis orthogonal to it, and how
  a value crosses that linkage.
- Where a defaulted formal's default is evaluated.
- How a subroutine an interface leaves to a connected module is reached.

## Does Not Own

- How a referencing site invokes or registers a callable -- a direct call, a fork spawn, a region
  submit, a constructor-time lifecycle registration. That is the referencing site's concern;
  [scheduling.md](scheduling.md) owns spawn and submission.
- The suspension protocol of a coroutine body ([scheduling.md](scheduling.md),
  [activation.md](activation.md)).
- Object and class layout, dispatch tables, and inheritance. How a call resolves its target --
  directly or by dynamic dispatch -- is a property of the call site and the object's class, owned by
  [object_model.md](object_model.md), not of the callable body.
- How the environment is populated -- which binding each field captures and how a binding crosses a
  callable boundary. That is [binding_and_capture.md](binding_and_capture.md).
- The runtime realization of a reference cell or an observable cell, and storage placement --
  runtime and LIR concerns.
- How the run finds the context a foreign entry runs in ([runtime_model.md](runtime_model.md)).
- The foreign interface beyond the callable: the standard's headers, linking foreign code, the
  boundary a design publishes, open arrays, the context extent and VPI
  ([foreign_interface.md](foreign_interface.md)).

## Core Invariants

1. **A callable is code; a callable value is code plus a bound environment.** A direct call
   references code and supplies every parameter per invocation, constructing no value. A callable
   value materializes only where a callable is deferred, stored, forked, submitted, or otherwise
   invoked later by a site that does not supply the environment. Captures belong to a callable
   value's construction, not to a code declaration, so one body can take different bound
   environments.

2. **A body entered on an object reaches it through the receiver binding its code states; a body
   entered on none has no receiver.** An instance method, a process and a constructor take their
   object as their first parameter, which the code names as its receiver rather than leaving a name
   or a position to say so, and every member access dereferences it and projects the field. A static
   method and a subroutine of a namespace are entered on no object and take none; what one needs
   that no object carries is an ordinary parameter. Whether a body has a receiver is a fact of its
   declaration, never of what its body happens to read, so adding a read of class state to a body
   changes no signature and no caller. A closure's own receiver is the closure, or the box it was
   made in, and the enclosing body's receiver is one captured field of it like any other; where the
   object lives while referenced, that field is the share of it the enclosing body holds, so a
   closure that outlives its method keeps the object alive
   ([binding_and_capture.md](binding_and_capture.md)). A body the runtime library enters on an
   object it holds as the root every object is states its receiver as a binding narrowed from that
   parameter. The receiver's type says whether the body borrows its object or takes over the box it
   was made in, read-only exactly where the receiver is. MIR states the receiver because a backend
   without a host-language `this` would otherwise re-derive which object a body runs on, and every
   backend would derive it again ([mir.md](mir.md) invariant 10).

3. **At a call, the object is part of the callee, never a position among the arguments.** A direct
   call states the object it is made on, or states none where the callee is entered on no object; a
   dispatched call always states one. The arguments are exactly the operands the source wrote, so no
   consumer counts or guesses which leading argument is a receiver. How the object reaches the
   callee in a target -- bound into a member call, or first in the operand list -- is that target's
   spelling. A qualified, non-virtual call of a base's method (LRM 8.15) is a direct call like any
   other, whichever unit declares the base.

4. **The result type is the call protocol.** A coroutine result type means the callable is entered
   and completed through the coroutine protocol -- the invocation yields a coroutine object the site
   must `await` or `spawn`. "Function versus task" and a process's coroutine-ness are the result
   type, never a kind tag. "The body can suspend" is a separate body-analysis fact, not the result
   type's meaning -- a `final` procedure that never waits is still entered through the protocol so
   the scheduler holds one shape.

5. **A completion yields one value, and awaiting is typed.** A function's return and its `output`
   and `inout` formals are the components of one completion payload: the return first, if the
   callable returns a value, then each `output` or `inout` formal in declaration order. The payload
   is the tuple of those components, the empty tuple where there are none, and `Coroutine<T>`
   carries it as `T`. Awaiting a `Coroutine<T>` yields a `T`, and each writeback is a projection of
   it by position, read off the callee's declaration. Whether a backend moves the payload through a
   slot the caller provides or through the coroutine object is its realization; such a slot is never
   a MIR parameter, which would reintroduce an alias for what is a copy.

6. **Parameter direction is data flow.** An `input` is a value parameter; a `ref` / `const ref` is a
   parameter of reference type, a live alias (LRM 13.5.2); an `output` is a component of the
   completion payload; an `inout` is a value parameter plus a payload component. There is no
   direction enum and no callable-level direction field. `output` and `inout` copy out at completion
   because their values ride the completion, written by the call site after it, so a caller never
   sees an intermediate write while a task is suspended (LRM 13.5) and an `output` has no copy-in.
   An `output` or `inout` actual place is bound once, at call entry, so a writeback after a
   suspension writes the place the call named; and the storage receiving a completion outlives any
   write the callee can still make to it.

7. **A `ref` formal is a reference bound once, as the call creates it.** HIR keeps `ref` as the
   direction the source wrote, since SystemVerilog has no reference type (LRM 13.5.2); HIR-to-MIR
   turns it into a parameter whose type is a reference, the generic-language way of saying a binding
   aliases storage elsewhere. The reference reads through the cell's read and writes through the
   cell's publication, so a write wakes what waits on the cell (LRM 9.4.2) -- which is why it is not
   a borrowed pointer. It is bound as the call creates it and never again, as C++'s `T&` is; a
   port's reference member is the other reference type, bound after its object is built and bound
   again by a force, as `std::reference_wrapper` is ([mir.md](mir.md)). Reading, writing and waiting
   through either reach the same storage; which operations may bind one is what differs.

8. **A bound field is a snapshot or an alias by its type.** A field whose type is a value type is an
   independent snapshot taken at construction; a field whose type is a reference type aliases the
   enclosing storage cell (reads see the current value, writes propagate, LRM 6.21). There is one
   binding mechanism -- the value owns a field -- and the field's type alone decides snapshot versus
   alias.

9. **A callable value's body is self-contained.** A callable value can outlive the frame that built
   it -- a spawned branch runs after its parent returns, a submitted effect runs in a later region.
   The body reaches outer state only through its bound environment and its parameters, never by
   climbing out of its own scope.

10. **Every callable carries its signature; a body is what a definition adds.** A declaration this
    program does not define simply has no body -- a DPI-C import, whose definition the user's
    foreign code provides, and a pure virtual method, which a deriving class supplies. An absent
    body is distinct from a present empty one, which is a definition that does nothing. Foreign
    linkage -- a fixed symbol name and the machine signature that name publishes -- is an
    independent axis over that: bodyless plus foreign is an import, bodied plus foreign is the entry
    point an export publishes, and pure-virtualness is bodyless plus a dispatch role. The whole
    variety falls out of independent facts on one declaration, and nothing tags a combination the
    structure already states.

11. **A callable's identity is its entry in its unit's one table, and its owner is whoever declares
    it.** The owners are the unit's namespace (a package or `$unit` subroutine, a DPI-C import or
    export, a body synthesized for what a type decides about a value of it), a class (a method, a
    process, a lifecycle body), a class's construction, a closure, and a record (an operation on its
    whole value). A computation a type owns -- an assignment pattern's `%p` text (LRM 21.2.1.6) --
    takes the value and no object, so the namespace owns it in every context; what encloses the site
    needing it decides nothing. Being reachable by an identifier is a relation the owner holds over
    the entry, stated only where the source declared a name, so a synthesized body takes no part in
    it, needs no minted name, and is unreachable from outside its unit because nothing published it.
    A call inside the unit names the entry; a call from another unit names what that unit published.

12. **A formal's default is evaluated by its callee.** A default "is evaluated in the scope
    containing the subroutine declaration each time a call using the default is made" and may read
    that scope's storage or the object a method is called on (LRM 13.5.3), so whatever declares a
    subroutine with a defaulted formal also declares, per such formal, a function evaluating the
    default, and states beside the formal which of its callables that is. A call omitting an
    argument states only that the default applies; it lowers nothing of the expression, so a change
    to a default rebuilds the callee's unit and no caller. The call evaluates the arguments the
    source wrote left to right, takes its object once into a share of its own, then evaluates each
    default left to right on that object -- the default of the method the call names, whichever body
    the object's class runs -- each handed the earlier formals by reference to what the call binds
    them, so a default may name an earlier formal, as the front end allows. A default of an
    `output`, `inout` or `ref` formal names a place, so its function returns a reference and the
    call treats the result as an actual it wrote. A constructor's default is evaluated on the object
    after creation and before the constructor (LRM 8.7, [object_model.md](object_model.md) invariant
    6); a DPI-C import's default is the declaring scope's.

13. **A subroutine an interface leaves to a module is a callback.** An `extern` task or function of
    an interface (LRM 25.7) is published as a callable with no body, beside whether it takes one
    definition or any number (`extern forkjoin`). For each, MIR holds an interface with one
    behavior, a compiler-made binding method on the interface's published class that keeps one
    handle or a queue of them, and a small class per definition implementing the behavior by calling
    the definition on the object it was made for; the definer binds it in its own resolve phase,
    before any initializer or process runs. Nothing in MIR says `extern`: the interface, the virtual
    call, the handle and the queue are what a class-based program already uses. A definition is a
    body of the defining module's own, so `disable` and the task's outputs behave as for any task
    (LRM 25.7.4).

## Boundary to Adjacent Layers

- HIR-to-MIR synthesizes callable code and callable values, fixing each signature, each result type
  (the call protocol), each receiver, and each bound field's type (snapshot versus alias). The
  compiler-synthesized closures are the lowering's encoding of a deferred, concurrent, watching, or
  ordering body; no SystemVerilog source construct is itself a closure.
- [binding_and_capture.md](binding_and_capture.md) owns how the environment is populated: which
  logical binding each field captures, how a binding crosses each callable boundary, and the
  carrier-versus-view choice the field's snapshot-or-alias type records. This doc owns the
  environment as structure; that doc owns what fills it.
- The referencing site decides use: a direct call, a fork that spawns each branch value, a region
  submit, or a constructor-time lifecycle registration (a process is an instance-bound callable
  value registered during construction; a finalization hook is a distinct registration from a
  startup one). The callable does not know which.
- [object_model.md](object_model.md) owns which body a dispatched call reaches and what a
  type-associated function is handed in place of an object; [lifetime.md](lifetime.md) owns that a
  body running on a managed object roots it.
- A backend realizes a callable from its signature, its environment, and its body by a fixed
  function of each node's kind and type ([backend_contract.md](backend_contract.md)): the
  snapshot-versus-alias of each bound field from its type, the call protocol from the result type,
  how a body is handed its object from its receiver's type. The parameter-versus-capture realization
  of a bound receiver and the symbol a foreign name is defined under are backend choices.

## Forbidden Shapes

Each entry names the requirement it fails.

- **A kind tag on a callable body** -- a function/task discriminator, a process kind, an async/sync
  flag, a "suspends" flag. It fails the one-statement rule ([mir.md](mir.md) invariant 10): the
  result type already states the protocol, and two facts that must agree drift apart.
- **An implementation-form species -- internal, external, prototype.** It fails the same rule: once
  every form carries its signature, the species restates only which of body presence, foreign
  linkage and a dispatch role are present.
- **A receiver derived from whether a body reads class state.** It fails incremental compilation
  ([north_star.md](north_star.md) invariant 4): the analysis is transitive, so an edit to one body
  would change the signature of every caller up the chain.
- **A receiver as a position in a call's argument list.** It fails [mir.md](mir.md) invariant 10:
  every consumer would decide for itself whether a first argument is the object, and a static method
  whose first formal is a handle is exactly where they disagree.
- **An expression meaning "the current receiver".** It fails the self-contained-body requirement of
  invariant 9: its meaning depends on which callable encloses it, which a consumer must walk out to
  discover.
- **A privileged capture slot for the enclosing receiver, or a per-form receiver mechanism.** It
  fails invariant 2: one receiver rule serves every form, and a closure binds the enclosing receiver
  as an ordinary field.
- **A parameter-direction enum or a callable-level direction field.** It fails invariant 6:
  direction is data flow, already stated by the signature.
- **An `output` or `inout` formal as a reference parameter.** It fails LRM 13.5: a live alias lets a
  caller observe an intermediate write while a task is suspended and copies a value into an
  `output`, which has no copy-in.
- **A reference realized as a borrowed pointer.** It fails LRM 9.4.2: a write through a raw address
  wakes nothing waiting on the cell.
- **A capture-kind axis beside the field type** -- a `by_reference` flag, or distinct by-value and
  by-reference capture nodes. It fails invariant 8: a parallel axis can contradict the type.
- **A body that reaches an enclosing variable by hopping out of its own scope.** It fails invariant
  9: the frame may be gone when the body runs.
- **A backend that constructs the reference wrapper itself** from a flag or from an operand that
  looks like an lvalue. It fails [mir.md](mir.md) invariant 10: the decision that a field aliases is
  made at HIR-to-MIR and carried in the field's type.
- **A lifecycle registration as a static class-level list.** It fails LRM 27: which generate blocks
  an instance built decides its processes, so registration is per-instance, constructor-time work,
  and a class holds at most a descriptor.
- **A caller lowering the callee's default expression.** It fails incremental compilation: the
  caller would compile the callee's text, and a change to a default would rebuild every caller with
  nothing on a signature recording why.
- **A callee evaluating its own defaults in a prologue, told by a hidden argument which were
  passed.** It fails LRM 13.5.3 read with LRM 8.20: an override would evaluate its own default where
  the call names another method's.
- **A synthesized body homed on a class where one happens to enclose the site, and on the namespace
  otherwise.** It fails invariant 11: a computation taking no object has one owner, and a two-shaped
  answer is carried by every consumer.
- **A name minted for a synthesized body.** It fails LRM 5.6.1: an identifier admits every printable
  character, so no spelling is reserved to mint one from without colliding with the source's.
- **A separate call family or declaration type for foreign calls.** It fails invariant 10: an import
  is a callable with no body and a foreign linkage, and a parallel family would give it a second
  render path.
- **An imported callable owned by a class.** It fails LRM 35.4: its name belongs to the DPI-C name
  space, which no class contains, and a package, which has no class, may declare one.
- **Marshaling realized by each backend from the foreign signature.** It fails
  [backend_contract.md](backend_contract.md)'s mechanical translation: the conversion would be
  written twice, against two runtime ABIs, and drift.
- **A carrier type in the SystemVerilog value set, or a MIR expression form for crossing the
  boundary.** It fails the closed value and expression sets of [mir.md](mir.md): a carrier is a
  machine or library type and the conversion an ordinary call.
- **An `extern` subroutine stated as a node or field of its own.** It fails [mir.md](mir.md)'s
  generic vocabulary: an interface, a virtual call and a handle already say it.

## Notes / Examples

A parameter's direction lowers entirely into the signature's data flow. A task

```text
task t(input int a, output int b, inout int c);
```

has the normalized contract `t : (self, int a, int c_in) -> Coroutine<(int b, int c)>`. The call
site supplies `a` and the snapshot of `c`, and after completion writes each payload component to the
caller's place, bound at entry. Every other formal, and a function's result variable (LRM 13.4.1),
is a variable of the lifetime the subroutine gives it. An automatic subroutine's is the body's own,
fresh at each call. A static subroutine's is one variable every call shares (LRM 13.3.2): its
instance's for a subroutine of the design hierarchy, and its namespace's for one of a package or the
compilation-unit scope, which every name reaching it names as it names the namespace's other
variables (LRM 6.21, 26.2). The body assigns its argument to it as it begins and the call reads its
output from it, so the data flow above is the same and only where the variable lives differs.

Closures across languages share the same core and differ only at the edges. C++ lambdas capture per
variable: `[x]` owns a copy of `x`, `[&x]` owns a reference to `x`; both are owned fields, and the
field's type (`T` versus `T&`) is the only difference. Rust closures are structs of owned captured
fields, a field's type being `T` (by value) or `&T` / `&mut T` (a borrow). Python, JavaScript, and
TypeScript have one mode -- they close over the enclosing cell -- because a garbage-collected
language keeps a cell alive as long as a closure refers to it. SystemVerilog is value-semantic, so a
captured snapshot (a value-typed field) is the default and an alias (a reference-typed field) is the
explicit case used when a body must share mutable storage with its environment.

A closure value is an anonymous concrete callable value
([compiler_generated_storage.md](compiler_generated_storage.md)): a per-closure-site `ClosureType`
of named capture fields plus one invoke body, made where its value lies and never moved -- the MIR
equivalent of a C++ lambda closure object. A backend realizes it as a type whose members are the
captures and whose one body reads each of them through the closure it runs against, the members
derived solely from the closure's fields, never from what the body happens to name. A
reference-typed field renders as an owned reference value, not a hidden C++ reference: a `[&name]`
reference dangles once the value outlives the captured variable's frame, and the reference wrapper
routes a write through the cell's update-event path, which a bare `T&` would bypass.

What the runtime is handed is not a backend's choice. The runtime is compiled before the program, so
a closure reaches it as an erased callable of its signature, through a box or a borrow, by a cast
MIR states ([compiler_generated_storage.md](compiler_generated_storage.md) invariant 10); a
backend's own callable wrapper never crosses that boundary.

**Foreign callables.** A DPI-C import is a bodyless callable of the unit's namespace with a foreign
linkage naming its symbol; it has no receiver, and a `context` import observes its declaration's
scope, which the call establishes around the foreign call rather than binding as a receiver. Because
the DPI-C name space is its own (LRM 35.4), a call reaches an import without naming a class or a
unit, and each calling unit holds its own copy of the declaration's prototype. The C prototype is
the callable's own signature -- one binding per formal, typed as the value crosses -- and is a fact
of the program-global name, stated on its linkage.

Marshaling is ordinary MIR calls of runtime conversion entries, expanded at each call site because
an open array's shape is the actual's (LRM 35.5.6.1): marshal each input to its carrier, call the
symbol, marshal each output back. A carrier is a machine type for a value Table H.1 passes in a
register and a library type for a canonical vector or an open array's image; on the calling side it
never outlives that one lowered statement and is never a user variable's type. A string an export
hands back is kept by the run until the import call that let the foreign caller in returns, as the
Java Native Interface keeps a local reference; an `inout` is written back only where its bits
changed, since SystemVerilog writes an address into an `inout` string only to effect a change (LRM
Annex H.8.10).

An export is an ordinary callable plus an entry point with the foreign linkage. A scope publishes an
entry taking the scope it runs against ahead of the C formals, and the symbol resolves the scope the
foreign side established (LRM 35.5.3) and calls that entry; a package's export needs no scope, so
its namespace defines the symbol directly. Each unit declaring such a scope defines the symbol, and
the link keeps one ([emission_model.md](emission_model.md) invariant 12). The headers, linking, open
arrays, the context extent and VPI are [foreign_interface.md](foreign_interface.md)'s.

**An `extern forkjoin` call** behaves as a `fork ... join` of every definition (LRM 25.7.4), whose
number is known only once the design is built, so its body is one branch that starts a `join_none`
branch per definition and then waits for them; the formals' lifetime is extended as for any formal a
`join_none` branch reads (LRM 6.21), and a call finding no definition is a run-time error reported
at the call. The callback class is rooted at the plain managed object, not the class-object root,
since nothing runs during resolve to give a class object its generator (LRM 18.14).

Current implementation: the C++ backend renders a body whose receiver borrows its object as a member
function whose first line binds `self` from `this`, a body that takes over its box as a static
function over the box, and a closure as a struct whose members are its captures with a call
operator.
