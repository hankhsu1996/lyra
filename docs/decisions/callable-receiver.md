# Callable receiver (explicit `self` first parameter)

Date: 2026-06-15 Status: accepted

## Context

Every SV callable (process, task, function, fork-branch closure, NBA / `$strobe` / `$sscanf` /
with-clause / deferred-check closure, constructor body) reads or writes the enclosing module's state
-- its signals, its child instances, its services. The body has no way to reach that state without a
pointer to where it lives; in software-semantic terms it must receive a pointer to the enclosing
class instance.

C++ provides a convenience for one shape of callable -- an instance method gets a `this` pointer for
free. The lyra backend leans on this for `mir::Process` / `mir::StructuralSubroutineDecl` bodies
(emitted as instance methods) and for the constructor body (the C++ constructor itself). For
closures the convenience does not apply -- a lambda has no `this`, so the backend smuggles the
receiver in either through `[this]` capture or through an explicit `(M* self, ...)` lambda
parameter. The choice today is mode-dependent and lives in the render: a fork-branch closure is
emitted with explicit `M* self`; an NBA / scan / with-clause closure is emitted with `[=, this]`
capture; `mir::StructuralVarRef` inside any body relies on render-time walker state
(`RenderContext::ReceiverObject()`) to know whether to spell the receiver `this` or `self`.

Three forces made the receiver model a real decision:

1. **MIR is the golden reference for multiple backends.** A future LIR / LLVM-IR backend will not
   have a C++ `this` keyword to lean on -- functions there take pointer parameters. If MIR keeps the
   receiver implicit, every backend re-runs the same "which receiver applies here" analysis, and the
   analysis is transitive (a helper that uses class state forces every caller to know too).
2. **Closure body must be self-contained.** A reader of the closure body's MIR cannot today tell
   whether structural-var access spells `this->X` or `self->X` -- the answer lives in the
   surrounding callable, propagated as render walker state. That is the "implicit context" smell:
   the body's meaning depends on where it sits, not on what it says.
3. **The implicit / explicit split is fragmenting.** The current backend has four mechanisms for
   receiver access (method `this`, fork-branch `(M* self)` param, NBA `[this]` capture, NBA `[=]`
   capture) that all answer the same question -- "how does this body reach the class instance". Each
   new closure family risks adding a fifth.

## Decision

**Every MIR callable carries a first parameter `self : Pointer<EnclosingScope>`. The parameter is
added unconditionally -- not derived from whether the body happens to touch class state. The body
reaches every class member through
`MemberAccessExpr(receiver = DerefExpr(ProceduralVarRef(self)), var = ...)`.**

This holds for `mir::Process`, `mir::StructuralSubroutineDecl`, `mir::ClosureExpr`, and the
constructor-body callable. The shape is uniform; no callable form is special.

The C++ backend renders the receiver shape with the most idiomatic C++ form available, but the form
is fixed by callable kind, not derived from context:

- **Closure** -> lambda whose explicit first parameter is `M* self`. Lambda capture clause is `[]`
  (empty); every value the lambda needs flows through its parameter list, not through implicit
  capture.
- **Process / subroutine / constructor-body** -> static method on the enclosing class whose first
  parameter is `M* self`. The class still owns the storage; method body lives on the class so it can
  name private members. The body sees `self->X`, never implicit `this`.

The constructor body is the only callable whose corresponding C++ entry cannot be static (the real
C++ constructor must be an instance constructor for instance creation to work). The constructor body
lowers to a static `init(M* self)` that the C++ constructor invokes with `init(this)`; the
constructor itself is the only place where C++'s implicit `this` is used at all, and only as the
argument to `init`. `CreateProcesses()` (and any other Scope-base virtual override) is C++ plumbing,
not an SV-derived callable; it remains a virtual instance method, and its body's implicit `this` is
C++-language-required, not MIR-modeled.

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

The render today carries walker state for the receiver (`ctx.ReceiverObject()`,
`ctx.MemberPrefix()`, `ctx.DeferredByValueCapture()`) and switches between four C++ mechanisms based
on which kind of body it has entered. Every future backend faces the same dispatch. Every
maintenance change risks the four-way switch going out of sync. MIR carrying the receiver as a real
parameter pushes the answer up to one place that every backend just reads.

## Rejected alternatives

- **Implicit `this` in methods, explicit `self` only in closures.** Matches C++ idioms most closely
  but leaves the four-mechanism fragmentation in place. A future LIR / LLVM-IR backend would have to
  re-derive "is this body a method or a closure" to know how to spell receiver access. The asymmetry
  buys idiomatic C++ at the cost of every other downstream.

- **Conditional `self` (only when the body reads class state).** Requires HIR-to-MIR to compute
  whether each body transitively touches class state -- a cross-call analysis that becomes wrong the
  moment a helper is added or removed. Saves a parameter the C++ optimizer would often elide anyway.

- **Body-local `auto self = this;` declaration as the first statement of every method body.** Looks
  uniform from the body's vantage but adds an emit-side declaration the user did not write. The MIR
  statement representing it would need a `ThisExpr` / `SelfScopeExpr` node that carries "give me the
  C++ `this` keyword", and that node is the same implicit-context smell this decision exists to
  remove (its meaning depends on the surrounding callable kind being "C++ instance method"). It also
  adds an extra `M*` local at every callable's entry that no optimizer is guaranteed to remove in
  debug builds.

- **`SelfScopeExpr` / `ThisExpr` as a MIR expression node** carrying "the current receiver". Has
  been the existing shape for fork-branch / NBA / scan closure bodies and is what the current
  `mir::SelfScopeExpr` is. The node's meaning is context-dependent -- it renders to `this` in a
  method body and `self` in a fork-branch body -- which is precisely the walker-state smell. The
  cleaner replacement is the explicit `self` parameter plus `ProceduralVarRef`.

- **A struct of free functions, no class membership at all.** Removes C++ implicit `this` entirely
  (process / subroutine bodies become free functions friended into the class). Plausible but breaks
  the natural C++ class-membership model that scheduler / runtime already use to reach instance
  methods through `Scope*` polymorphism. Static methods give the same explicit-receiver shape
  without losing class membership.

## Consequences

- `mir::Process`, `mir::StructuralSubroutineDecl`, `mir::ClosureExpr`, and the constructor body each
  declare a `self` binding at `body.vars[0]` whose type is the unique-pointer / borrowed- pointer to
  the enclosing structural scope. The binding's name is `self`; the type makes the pointee
  unambiguous.

- A new MIR expression `MemberAccessExpr { receiver: ExprId, var: StructuralVarRef-fields }`
  replaces today's implicit-receiver `StructuralVarRef`. The receiver expression is rendered before
  the access; the access itself is mechanical (`render(receiver) + "->" + var_name`).

- `mir::SelfScopeExpr` is removed.

- The cpp backend's `RenderContext::ReceiverObject()`, `WithReceiver(...)`, `MemberPrefix()`,
  `ServicesRef()`, and `DeferredByValueCapture()` are removed. Receiver-related walker state on
  `RenderContext` is gone.

- C++ emit shape changes: process / subroutine / constructor bodies emit as
  `static auto <name>(M* self, ...) -> ... { ... }`; the C++ constructor delegates the body to a
  static `init(this)` call. Lambda capture clauses are `[]` -- every captured value flows through
  the lambda's parameter list. `CreateProcesses()` adapts: where it previously emitted
  `AddProcess(kind, process_N())` it now emits `AddProcess(kind, process_N(this))`.

- LIR / LLVM-IR lowering reads the receiver parameter directly off MIR. No backend re-derives
  receiver semantics.

- Forbidden Shape: a MIR expression whose meaning depends on which callable encloses it. The
  receiver is on the callable's parameter list, not on the expression.

## Cross-references

- `docs/architecture/mir.md` (callable shape, expression set)
- `docs/progress/refactor.md` R16
