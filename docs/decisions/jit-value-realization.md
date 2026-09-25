# JIT value realization: opaque handles plus a generated-call scope

Date: 2026-07-07 Status: accepted; the lifetime half superseded 2026-09-24

**Superseded in part** by [a-value-lives-in-its-makers-frame](a-value-lives-in-its-makers-frame.md).
Its premise below -- "generated LLVM code cannot construct or own a runtime value object; it can
only hold a pointer to one" -- does not hold: clang-generated code constructs and owns these very
objects given their size, alignment, and out-of-line constructor and destructor, and every value
domain has one size whatever source type it stands for. A value now lives in the generated body's
own frame and ends at the end of its full-expression; `GeneratedCallScope` owns no values. The
per-call region was not an optimization gap but a correctness defect -- memory grew with the work
done between two waits. Values still cross as pointers and the runtime still performs every
operation on them; that half stands.

## Context

The runtime represents every SystemVerilog value as a runtime-library object -- a `PackedArray`, a
`String`, a `PrintItem`, a `Coroutine`. The C++ backend emits C++ that holds these as ordinary stack
objects; their lifetime is the C++ language's (stack scope and RAII), and their operations are
direct method calls the C++ compiler can inline.

A JIT/LLVM backend has no C++ stack objects. Generated LLVM code cannot construct or own a runtime
value object; it can only hold a pointer to one. So the JIT crosses the runtime boundary through an
`extern "C"` ABI in which every runtime value is an opaque handle (`void*`): the runtime library
constructs the value, performs every operation on it, and hands the generated side a handle. Every
value type is uniform here -- `String`, `PackedArray`, `PrintItem`, `Coroutine` are all opaque
handles; the generated code shuffles handles and calls the ABI, and the same runtime library the C++
backend uses does the actual work.

This raises two questions. First, who owns the transient value objects the generated code creates
during one call, and when are they freed -- the generated side holds only handles and can neither
own nor free them. Second, whether this opaque-handle scheme is the intended value realization or
whether generated code must instead lower values into an LLVM-native, in-frame byte representation
(a physical-layout derivation from the LIR value types) before the JIT can proceed.

## Decision

The opaque-handle scheme is the **baseline runtime-value realization** for the JIT, and
`GeneratedCallScope` owns the transient runtime values a single generated entry creates. A
physical-layout / in-frame value lowering is a **later optimization path, not a correctness
prerequisite**.

- **Opaque handles are the baseline JIT value ABI.** Generated code carries `void*` handles; the
  runtime library owns and operates on the value objects. This is not a design hack: it is the
  natural counterpart of the opaque-handle runtime ABI. The C++ backend leans on the C++ stack and
  RAII to manage temporaries; the JIT, having no C++ stack objects, needs a runtime-managed
  temporary scope to play the same role.

- **`GeneratedCallScope` owns transient values of one generated entry.** The runtime pushes a scope
  around each runtime-to-generated call (the construct entry, each lifecycle body, each resumed
  process). Values the ABI materializes during that call live in the scope's arena and are released
  together when the call returns. The C++ backend enters the same scope and allocates nothing.

- **Physical-layout / in-frame value lowering is future work.** Selectively lowering plain values
  into an LLVM-native, in-frame representation -- so the value's bytes live in the generated
  function's own frame and LLVM can inline and fold value operations -- is an optimization and
  maturity path. It is not required for the JIT to be correct, and this decision does not gate JIT
  progress on it.

The accurate framing, which documentation must not overstate into a final value model:

> The opaque-handle JIT ABI is the baseline runtime-value realization. `GeneratedCallScope` owns the
> transient runtime values created during one generated entry. Physical-layout / in-frame lowering
> is a later optimization path, not a correctness prerequisite.

### Invariants

1. **`GeneratedCallScope` manages only transient values of the current generated entry.** It does
   not own values that must cross a call, cross a suspension, or live as an instance member, signal
   storage, or a process-persistent local.

2. **A handle that must outlive the call is explicitly consumed, copied, or promoted by a runtime
   API into longer-lived storage.** For example, `register_initial` consumes the coroutine wrapper;
   the persistent object is the `Coroutine` moved into the `RuntimeProcess`, not the transient
   wrapper the arena held.

3. **A future cross-suspension local value does not live in `GeneratedCallScope`.** That class of
   value needs an activation frame / process frame; if it carries a managed edge, that storage is
   runtime-traceable per `lifetime.md`.

4. **The scheme does not pollute the LIR type system.** LIR describes logical values; the `void*`
   handle is one realization of the JIT/runtime ABI, below LIR, never a LIR-level type.

5. **`PackedArray` stays an opaque handle, consistent with `String` and the dynamic aggregates.**
   Native lowering of small scalars or simple packed values is a future option, not a split to make
   now.

6. **A runtime value handle is immutable from the generated side's view; every apparent mutation is
   functional.** A whole-value assignment carries a handle, which a copy may alias, so the runtime
   value object behind it is never mutated in place through a possibly-shared handle. An operation
   that appears to mutate a value -- a scalar compound-assign, a container element write, a
   receiver-mutating container method (`delete`) -- produces a _new_ value handle, which is stored
   back through the value's owner (its place, activation cell, or observable cell). This is what
   makes handle aliasing safe: `b = a; a[i] = x;` leaves `b` intact because the element write
   rebuilds `a`'s value and restores it to `a`'s storage, never touching the object `b` still holds.
   The C++ backend reaches the same value semantics through the language's own value copies, so it
   may mutate a local value object in place; the generated side, holding only handles, may not, and
   never copies that in-place realization.

## Separate from the generated-behavior descriptor

Value lifetime and ABI realization is one axis; the ownership of the generated-behavior record
(`ScopeProgram` / `ScopeDefinition`) and its backend-contract shape is a different one
([generated-behavior-boundary](generated-behavior-boundary.md)). How a unit's dispatch record is
constructed -- and that a backend must not fabricate it in render -- is decided there, not here. The
two are not to be reasoned about together.

## Rejected

- **Requiring physical-layout / in-frame value lowering before the JIT proceeds.** This makes a
  performance-and-maturity capability a correctness gate. The opaque-handle scheme is already
  correct and uniform across value types; blocking on in-frame lowering delays every downstream
  feature for a benefit (LLVM value optimization, no per-value runtime call) that is not needed for
  correctness. Revisit as an optimization once the correctness baseline is broad.

  **That condition has fired, and what it turned up is not the optimization it anticipated.** Every
  non-managed value domain is realized now. What the breadth exposed is in the lifetime half of this
  decision rather than the ABI half: `GeneratedCallScope` owns one entry's transients, and two more
  regimes have since been added beside it for values it cannot own -- one for a value that outlives
  a stretch, one for a value a reference must be able to address. Three regimes for one concept, and
  which one a variable gets follows from properties of the enclosing body rather than of the
  variable.

  Why they could accumulate is invariant 6, stated here from the beginning: a handle may be aliased
  by a copy, so nothing writes into a value object and holding one implies no ownership. Measurement
  on 2026-09-03 confirms the invariant still holds in the emitted code -- two live source locals
  share one handle, and a later assignment to one replaces the handle rather than the object. What
  the measurement adds is the asymmetry beside it: storage elsewhere in this project already holds
  its value by value rather than by handle, so the ownerless case is the procedural local on this
  backend and nothing else.

  So the revisit is open, and it is a question about ownership rather than about layout: whether a
  place owns a value's representation or holds a handle to an independently lived one -- invariant 6
  being the written form of the second answer, taken when only the transient case was in view. The
  ABI half of this decision -- values crossing as opaque handles, the runtime performing every
  operation -- is not what is in question and is not changed by either answer.

  **Answered 2026-09-11 by [storage-owns-its-value](storage-owns-its-value.md)**: a storage entity
  owns the representation of its current value, so invariant 6 no longer describes the aggregates
  whose components the language gives an identity of their own. Value semantics there come from
  copying at assignment rather than from an immutable representation. The ABI half stands, as this
  paragraph anticipated.

- **A per-type special case for `PackedArray` (in-frame while others stay handles).** `PackedArray`
  is already an opaque handle like every other value type; carving it out now trades the uniform ABI
  for a partial lowering with no correctness payoff. Native small-scalar lowering, if pursued, is a
  later uniform optimization, not a present asymmetry.

## Consequences

- `GeneratedCallScope` stays, documented as the owner of one generated entry's transient runtime
  values; it is the JIT counterpart of C++ stack/RAII temporary lifetime.
- Cross-suspension and managed-value lifetime is explicitly out of `GeneratedCallScope`'s scope; it
  is future activation-frame work, and reaching a design there does not change this baseline.
- Documentation describes the opaque-handle scheme as the baseline realization and physical-layout /
  in-frame lowering as a later optimization, never as the final or only value model.

## Cross-references

- `architecture/lifetime.md` (language-visible state lives in Lyra-owned storage; managed values are
  runtime-traceable -- the opaque-handle scheme keeps value state in runtime storage, and
  cross-suspension managed values need traceable frames).
- `architecture/lir.md` (LIR carries logical value types; physical layout is a separate derivation
  below LIR -- the future optimization path).
- [generated-behavior-boundary](generated-behavior-boundary.md) (the separate descriptor-ownership
  and backend-contract axis).
