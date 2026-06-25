# Address-of primitive in MIR

Date: 2026-06-23 Status: accepted

## Context

MIR has long carried `DerefExpr` (pointer -> place) but no operator for the dual direction (place ->
pointer). The shape that filled the gap was render-time injection: the C++ backend, on seeing a
runtime call whose signature takes a pointer (`RegisterSignal(void*)`,
`WaitAny({Observable*, ...})`), looked at the call's argument, recognised it was a
`MemberAccessExpr`, and prepended `&`. For an `ExternalRef` member the same render switched to
`<member>.AsObservable()`. The choice of shape lived in render; MIR's argument vector was a bare
`MemberAccessExpr` in every case.

This violates `mir.md` invariant 10 (a backend never re-derives a fact MIR did not state). The
violation is masked in the C++ backend because C++ has the `&` operator and reference-parameter
auto-binding. Any other backend has to redo the same reasoning -- LLVM IR has no C++ reference sugar
and would, given the same MIR, independently decide to take the address.

## Decision

**MIR grows `AddressOfExpr` as the dual of `DerefExpr`.** A single-operand node that takes a place
expression and yields a borrowed pointer to that storage:

```text
AddressOfExpr { operand: ExprId }
type: PointerType { pointee = operand.type, ownership = kBorrowed }
```

The operand must be an addressable place (`LocalRef`, `MemberAccess`, `DerefExpr`, or any
place-producing access primitive). Value expressions are not addressable; the verifier rejects them.

Two HIR-to-MIR lowering sites are immediately reshaped to use the new primitive:

1. **`kRegisterSignal`** -- the second argument is wrapped in `AddressOfExpr` at lowering time. The
   render path stops prepending `&`.

2. **Sensitivity wait leaves** -- the leaf carries an explicit observable-pointer expression chosen
   by lowering, one of three shapes by the leaf's MIR type:
   - Plain value member (`ObservableType<T>`): `AddressOfExpr(MemberAccess(self, member))`.
   - Borrowed-pointer slot (`PointerType{kBorrowed}`, the cross-unit-ref case): bare `MemberAccess`
     -- the slot already holds the pointer.
   - `ExternalRefType` member: `CallExpr(BuiltinFn::kAsObservable, [MemberAccess(self, member)])` --
     the runtime resolves the upward reference to its current observable.

The render-side type-kind dispatch (`RenderSensitivityRefPtr`) disappears. Render walks the
expression tree the lowering already shaped.

### Canonicalization

`AddressOf(Deref(p))` collapses to `p` -- mechanically applied by each backend's lowering of
`AddressOfExpr`. The C++ backend skips the `&(*p)` round-trip and emits `p` directly. LLVM backends
drop the round-trip the same way.

### Borrow kind

Lyra has no borrow checker and no source-level aliasing or lifetime system. `AddressOfExpr` does not
encode a shared / mutable distinction today. The runtime ABI takes `Observable*` and the distinction
would have no consumer. If a later borrow / lifetime system arrives, the node can grow a
`BorrowKind` field without callers having to migrate; until then the absence is principled, not
provisional.

## Rejected alternatives

### A. "Runtime takes references instead of pointers; no AddressOf needed"

Initially preferred. The runtime API changes from `RegisterSignal(name, void*)` to
`RegisterSignal(name, Observable&)`. The render emits a bare `MemberAccess`; C++ auto-binds the
reference parameter; for cases where the runtime stores the address internally
(`SubscriptionRequest` in an array passed to `WaitAny`), a wrapper struct's constructor takes
`Observable&` and stores `&observable` in a private field. No render-side `&`, no MIR-level
operation.

Rejected because the address-taking is not eliminated; it is hidden inside C++ language sugar
(reference parameter binding). The LIR -> LLVM backend has no reference sugar to hide behind: it
must (1) recognise that the runtime ABI takes a pointer-shaped parameter, (2) treat the
`MemberAccess` argument as a place, and (3) emit the address-taking lowering. That is exactly the
"every backend re-derives the same fact" failure invariant 10 calls out. Position A is clean only in
the C++ backend's spelling; the underlying violation is unchanged and propagates to every future
backend.

### B. "Python / TypeScript don't have AddressOf, so MIR shouldn't either"

The original framing. Rejected because Python / TS lack AddressOf as a consequence of having no
notion of memory locations at all -- variables bind to heap values, there is no place model. MIR is
not in that family. MIR already carries `PointerType { kBorrowed }`, `DerefExpr`, `self : M*`, and
`MemberAccess` as a place primitive. The closer reference class is Rust MIR (which has the
`Rvalue::Ref` / borrow operator) and LLVM IR (which does not need a separate operator only because
every variable is already an `alloca` -- effectively pre-typed as pointer). Both languages that
share MIR's value model state the place-to-pointer conversion explicitly.

### C. Generic conversion node (extend `ConversionExpr` to cover place-to-pointer)

Considered briefly. Rejected because `ConversionExpr` models value-to-value translations (numeric,
signedness, width). A place-to-pointer step is structurally distinct -- it changes the expression's
category (place -> value of pointer type), not just its numeric encoding. Folding both into one node
forces every consumer to disambiguate per case; keeping them as separate node kinds keeps the
structural fact explicit.

## Consequences

- Both backends consume the same MIR; neither runs a "is this argument a place that needs its
  address taken" analysis. The C++ render emits `&place` (or skips the round-trip via the
  canonicalization); the LIR / LLVM backend emits a GEP-style pointer computation. Both reach the
  same emitted shape mechanically.

- The sensitivity-leaf type-kind switch (`RenderSensitivityRefPtr`) is gone. The three
  observable-pointer shapes (`AddressOf(MemberAccess)`, bare `MemberAccess`,
  `Call(kAsObservable, ...)`) are chosen at lowering, stored as an `ExprId` on `SensitivityRead`,
  and rendered through the ordinary expression path.

- A future audit of the `RuntimeNavCallee::kGetSignal` `void*` -> `static_cast<Var<T>*>` injection
  is now scoped: the right fix is to make the runtime API return the typed pointer (eliminating the
  cast), not to grow a generic `CastExpr` primitive in MIR. The principle the engineer flagged --
  avoid `void*` at the MIR/runtime boundary because it forces every backend to re-derive the cast --
  is the same one this decision rests on.

- Render-side scaffolding that produces an entire C++ method declaration as a string template (e.g.
  `TimePrecisionPower()` virtual override) stays "C++ plumbing" under
  `decisions/callable-receiver.md`'s precedent. The sharpened boundary: plumbing is allowed only
  when it does not change the semantic shape of any MIR operand and does not require the backend to
  derive a new value-level operation. The `&` injection failed both tests; class-layout boilerplate
  that mechanically materialises an already-MIR-stated fact does not.

## Cross-references

- `architecture/mir.md` invariant 10 (a backend never re-derives a fact MIR did not state) and the
  closed-primitive-set property (the set grows only for a genuinely new generic-language concept).
- `decisions/value-type-concepts.md` (the parallel lift of `Var<T>::Get` / `Set` / `Mutate` from
  implicit render to explicit MIR `CallExpr` -- same family of fix on a different axis).
- `decisions/callable-receiver.md` (the "C++ plumbing, not MIR-modeled" precedent for virtual
  overrides, sharpened above).
