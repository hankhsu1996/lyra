# Callable receiver (explicit `self` first binding)

Date: 2026-06-15 Status: accepted. Revised 2026-10-29. How a body is handed its object is read from
its receiver's type.

## Context

Every SV callable (process, task, function, fork-branch closure, NBA / `$strobe` / `$sscanf` /
with-clause / deferred-check closure, constructor body) reads or writes the enclosing module's state
-- its signals, its child instances, its services. The body has no way to reach that state without a
pointer to where it lives; in software-semantic terms it must receive a pointer to the enclosing
class instance.

C++ provides a convenience for one shape of callable -- an instance method gets a `this` pointer for
free. The lyra backend leans on this for `mir::Process` / `mir::MethodDecl` bodies (emitted as
instance methods) and for the constructor body (the C++ constructor itself). For closures the
convenience does not apply -- a lambda has no `this`, so the backend smuggles the receiver in either
through `[this]` capture or through an explicit `(M* self, ...)` lambda parameter. The choice today
is mode-dependent and lives in the render: a fork-branch closure is emitted with explicit `M* self`;
an NBA / scan / with-clause closure is emitted with `[=, this]` capture; `mir::MemberRef` inside any
body relies on render-time walker state (`RenderContext::ReceiverObject()`) to know whether to spell
the receiver `this` or `self`.

Three forces made the receiver model a real decision:

1. **MIR is the golden reference for multiple backends.** A future LIR / LLVM-IR backend will not
   have a C++ `this` keyword to lean on -- functions there take pointer parameters. If MIR keeps the
   receiver implicit, every backend re-runs the same "which receiver applies here" analysis, and the
   analysis is transitive (a helper that uses class state forces every caller to know too).
2. **Closure body must be self-contained.** A reader of the closure body's MIR cannot today tell
   whether member access spells `this->X` or `self->X` -- the answer lives in the surrounding
   callable, propagated as render walker state. That is the "implicit context" smell: the body's
   meaning depends on where it sits, not on what it says.
3. **The implicit / explicit split is fragmenting.** The current backend has four mechanisms for
   receiver access (method `this`, fork-branch `(M* self)` param, NBA `[this]` capture, NBA `[=]`
   capture) that all answer the same question -- "how does this body reach the class instance". Each
   new closure family risks adding a fifth.

## Decision

**Every MIR callable body's first binding is `self : Pointer<Self>` -- `locals[0]` is a local
declaration of that pointer type, named `self`. The binding is added unconditionally, not derived
from whether the body happens to touch class state. The body reaches every class member through
`MemberAccessExpr(receiver = DerefExpr(LocalRef(self)), var = ...)`. The body's read of `self` is
identical across every callable form -- a backend reads `locals[0]` the same way in a process, a
method, a constructor body, and a closure.**

Where `self`'s value comes from differs by callable form, following each form's natural binding
shape:

- **Method-form callables** (process, method, constructor body) take `self` as the first formal
  parameter. Each caller knows its own receiver -- the runtime constructing a process coroutine, a
  call site invoking a method, the C++ constructor running the init body -- and supplies it at
  invocation. The body's binding is filled by the call.
- **Closure** binds `self` -- the code's first parameter (`code.params[0]`, landing at the body's
  `locals[0]`) -- into its environment, the enclosing scope's read of its own `self` supplying the
  bound value at construction. `self` is an ordinary environment field, bound like any other
  captured variable, not a privileged slot (see `unified-callable-model.md`). This holds for every
  closure form, the fork branch included. No invoker of a closure value -- the NBA /
  postponed-`$strobe` / deferred-check region runner, the fork scheduler, a sort handed an order --
  can be relied on to know which receiver belongs in the body, because a closure value can outlive
  the enclosing-scope frame that knows. The enclosing scope snapshots its own `self` into the
  closure's environment at construction; the body reads the bound value. Where the object lives
  while referenced, what the scope snapshots is the share of the object it holds, and the body's
  `self` is derived from that share where the body begins, so a closure that outlives its method
  keeps the object alive (`../architecture/binding_and_capture.md` invariant 9).

This partition is not a stylistic choice -- it follows from "who knows the receiver?". A method-form
callable has a caller-who-knows; a closure does not. Forcing both forms onto one supply mechanism
would either require the closure caller to learn a receiver it cannot know, or require
`std::bind_front`-style wrapping at every closure submit site that re-encodes the same snapshot
under a different name. Each form using its natural mechanism yields one C++ idiom per form and zero
special-case wrapping.

This holds for `mir::Process`, `mir::MethodDecl`, `mir::ClosureExpr`, and the constructor-body
callable. The body shape is uniform; the supply mechanism differs.

The C++ backend renders each form in its natural idiom. How a body is handed its object is one
answer of the type mapping, read from the type of the body's receiver binding, for a class's method,
a record's method, a constructor and a closure alike (`../architecture/backend_contract.md`):

- **A receiver that borrows its object** is C++'s own `this`. The body is a member function of its
  class, read-only exactly where the receiver is, and its first line binds the receiver from what
  C++ hands it -- `C* self = this;` for an object pointed at, `const S& self = *this;` for one
  referred to. The body then sees `self->X`, as every other body does.
- **A receiver that takes over the box it was made in** is the first parameter of a static function,
  so whatever the box holds lives for as long as the body runs.
- **Closure** -> a struct whose members are the captures, built as
  `sv_closure_<n>{self, cap1, lyra::runtime::VariableRef<T>(<lvalue>)}` -- a by-reference capture
  binds a `VariableRef<T>` rather than a snapshot. Its invoke reads each capture through the closure
  it runs against, the way the MIR invoke reads it through its receiver, so the captured `self` is
  reached as `closure->self->X`, never through an implicit `this`. A coroutine closure (a fork
  branch) is made in a box, and its receiver takes that box over, so the execution owns its
  captures.

A constructor is the one body it is. The object is created whole first, and the constructor is a
member function entered on it that binds its receiver on its first line like every method, so a
class has no second callable for it and no entry forwarding into one.

Revised on 2026-10-29. This section used to render every method-form body as a static function over
`M* self`, with the C++ constructor and the lifecycle hooks as thin instance entries forwarding to
it, and the rejected alternatives below turned down a first-line `self = this` binding. Both were
answers to a body whose receiver was only a parameter. Once every body states its receiver binding,
binding it from `this` on the first line is that statement spelled in C++, with no node standing for
the C++ keyword and no shell around the body. How a body is handed its object had meanwhile grown
three answers -- a closure's from its kind, a member's binding from whether its receiver was a
reference, its qualifier from a reference's mutability alone -- under which a read-only receiver
held through a pointer would have lost its qualifier with nothing refusing it. So it is one answer,
read from the receiver's type.

### Child-instance construction receiver triple

When a parent scope instantiates an owned child or external-unit child, the call carries the
parent's `self`, the child's structural `HierarchySegment` (label plus per-dim elaborated indices),
and the engine handle as the first three arguments before any user-supplied structural parameters.
The MIR shape is one explicit `CallExpr` against `ConstructorCallee` whose result type is
`unique_ptr<Child>` and whose arguments are `[self, segment, services, structural_args...]`. The
render is mechanical: it reads the unique-pointer result type and emits
`std::make_unique<Child>(self, HierarchySegment{...}, services, structural_args...)`. After the
construction returns its `unique_ptr<Child>` -- assigned to the slot for a scalar member, pushed
into the vector for an array element -- an explicit attach call hands it to the runtime tree, under
the parent its construction was given. Identity rides on the child (its segment is the single source
of `%m` and debug output); the parent never re-states the label or indices. Neither the receiver
triple nor the attach call lives in any stmt-kind dispatch; both are ordinary `CallExpr` nodes, and
every backend reads them the same way.

### Why always include `self`, not derive from "body touches class state"

The derivation is transitive: a helper that touches class state forces every caller to take a
receiver and to forward it. Tracking "this body needs class state" through arbitrary call chains is
the same kind of cross-cutting analysis that escape-analysis is in higher-level languages --
expensive, hard to keep correct under refactors, and a constant source of debugging surprise when
the analysis says one thing and the runtime needs another. Always-include trades 8 bytes of unused
pointer in the rare case the body genuinely never needs `self` for zero analysis complexity at
HIR-to-MIR and zero per-backend re-derivation downstream. The C++ build already suppresses
`-Wunused-parameter`; a future LIR backend can let unused parameters be elided by its own
constant-propagation pass if the cost ever matters.

### Why MIR explicit, not "let render decide"

The render carried walker state for the receiver (`ctx.ReceiverObject()`, `ctx.MemberPrefix()`,
`ctx.DeferredByValueCapture()`) and switched between four C++ mechanisms based on which kind of body
it had entered. Every future backend faces the same dispatch. Every maintenance change risks the
four-way switch going out of sync. MIR carrying the receiver as a real binding pushes the answer up
to one place that every backend just reads.

## Rejected alternatives

- **Implicit `this` in methods, explicit `self` only in closures.** Matches C++ idioms most closely
  but leaves the four-mechanism fragmentation in place. A future LIR / LLVM-IR backend would have to
  re-derive "is this body a method or a closure" to know how to spell receiver access. The asymmetry
  buys idiomatic C++ at the cost of every other downstream.

- **Conditional `self` (only when the body reads class state).** Requires HIR-to-MIR to compute
  whether each body transitively touches class state -- a cross-call analysis that becomes wrong the
  moment a helper is added or removed. Saves a parameter the C++ optimizer would often elide anyway.

- **A MIR statement binding `self` to the C++ `this` keyword at the top of every method body.** It
  would need a `ThisExpr` / `SelfScopeExpr` node that carries "give me the C++ `this` keyword", and
  that node is the same implicit-context smell this decision exists to remove (its meaning depends
  on the surrounding callable kind being "C++ instance method"). The C++ backend does write such a
  binding today, but as its spelling of how C++ hands a body the object its receiver binding states,
  read from the binding's type; MIR states only the binding.

- **`SelfScopeExpr` / `ThisExpr` as a MIR expression node** carrying "the current receiver". Has
  been the existing shape for fork-branch / NBA / scan closure bodies and is what the current
  `mir::SelfScopeExpr` is. The node's meaning is context-dependent -- it renders to `this` in a
  method body and `self` in a fork-branch body -- which is precisely the walker-state smell. The
  cleaner replacement is the explicit `self` binding plus `LocalRef`.

- **A struct of free functions, no class membership at all.** Removes C++ implicit `this` entirely
  (process / method bodies become free functions friended into the class). Plausible but breaks the
  natural C++ class-membership model that scheduler / runtime already use to reach instance methods
  through `Scope*` polymorphism. Member functions binding `self` on their first line give the same
  explicit receiver without losing class membership. (This rejection's `Scope*`-polymorphism premise
  is revisited by [generated-behavior-boundary](generated-behavior-boundary.md): the runtime reaches
  generated behavior through a backend-neutral unit definition rather than `Scope*` virtual
  dispatch, so class membership is not required for dispatch. The explicit `self` binding this
  decision fixes is retained there; whether a body is emitted on the class is a backend-private
  realization detail.)

- **`self` as a closure parameter, supplied at invocation via `std::bind_front` at every deferred
  submit site.** The literal "every callable receives self the same way" path: closures get an
  `M* self` formal, lambda capture is `[]`, deferred submission wraps with
  `std::bind_front(lambda, this, ...)` to bind the receiver before the runtime invokes the (now
  zero-arg) callable. Looks uniform on paper but mistakes the question. The closure value's receiver
  is fixed at the moment the value is built -- the runtime that later invokes it has no other
  receiver to choose. Calling that fixed value a "parameter supplied at invocation" forces a
  wrapping layer (`std::bind_front`) at every submit site whose only job is to re-encode
  "construction-time snapshot" under the name "parameter". Semantically the closure's receiver IS a
  capture; insisting on the parameter shape is shape-fitting, not modeling, and the wrapping shows
  up at every deferred site. The chosen shape -- `self` as the closure's first by-value capture --
  models the actual lifetime: the enclosing scope holds the receiver, the closure snapshots it at
  construction, and the body reads the snapshot. No `std::bind_front`, no `[this]`, no parameter
  that never varies.

## Consequences

- `mir::Process`, `mir::MethodDecl`, `mir::ClosureExpr`, and the constructor body each declare a
  `self` binding at `locals[0]` whose type is the borrowed-pointer to the enclosing class. The
  binding's name is `self`; the type makes the pointee unambiguous.

- `self` is the code's first parameter (`code.params[0]`) for every callable, landing at
  `locals[0]`. For method-form callables (process, method, constructor body) the caller supplies it
  at invocation. For a closure -- every closure, the fork branch included -- the closure value binds
  it into its environment, the enclosing scope's read of its own `self` supplying the bound value at
  construction. It is an ordinary environment field, not a privileged slot
  (`unified-callable-model.md`).

- A new MIR expression `MemberAccessExpr { receiver: ExprId, var: MemberRef-fields }` replaces the
  implicit-receiver member reference. The receiver expression is rendered before the access; the
  access itself is mechanical (`render(receiver) + "->" + var_name`).

- `mir::SelfScopeExpr` is removed.

- The cpp backend's `RenderContext::ReceiverObject()`, `WithReceiver(...)`, `MemberPrefix()`,
  `ServicesRef()`, and `DeferredByValueCapture()` are removed. Receiver-related walker state on
  `RenderContext` is gone.

- C++ emit shape: a body whose receiver borrows its object emits as a member function whose first
  line binds the receiver from `this` -- `auto C::<name>(params) -> R { C* self = this; ... }` --
  and one taking over its box as `static auto <name>(Box self, params) -> R`. A closure emits as a
  struct whose members are its captures, every one name-explicit and by value; an alias capture
  binds a reference value, never a C++ `[&]` reference (see `docs/architecture/callable.md` for
  why). The body that creates a scope's processes registers each one over the receiver it was
  handed, the same way every other body reaches its own.

- LIR / LLVM-IR lowering reads the `self` binding directly off MIR. No backend re-derives receiver
  semantics.

- Forbidden Shape: a MIR expression whose meaning depends on which callable encloses it. The
  receiver is reached through `locals[0]` as the `self` binding, not through an expression that
  means "look around and figure it out".

## Cross-references

- `docs/architecture/callable.md` (closure model: capture, references as a field type,
  coroutine-ness)
- `docs/architecture/mir.md` (callable shape, expression set)
