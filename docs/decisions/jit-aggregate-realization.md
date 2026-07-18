# Aggregate values are runtime-owned opaque values on the JIT, not monomorphized

Date: 2026-07-18 Status: accepted

## Why this decision matters

The execution (JIT/LLVM) backend realizes every runtime value as an opaque handle into a
runtime-library object ([jit-value-realization](jit-value-realization.md)). For a scalar this is
pure reuse: one runtime-fat class already carries every shape of the value
([integral-representation](integral-representation.md)), so both backends share it and the JIT
constructs nothing new. An aggregate is not pure reuse. The value layer realizes an aggregate as a
C++ template parameterized on its element types (`Tuple<Ts...>`, `DynamicArray<T>`, `Queue<T>`,
`AssociativeArray<K, V>`), which the C++ backend monomorphizes by leaning on the host C++ compiler.
The JIT emits LLVM IR directly and runs no C++ front-end, so nothing monomorphizes those templates
for it; it must choose a realization.

Two realizations are possible, and the choice governs the whole aggregate family, so it is settled
here rather than re-argued per container.

## The realization levels

- **Erasure.** One runtime-owned, type-erased value object behind a single opaque handle: a struct
  is `value::RuntimeTuple` (a `vector<value::RuntimeValue>` owning its components by value), reached
  through one handle; every operation is a runtime-library call.
- **Structural monomorphization.** Codegen specializes each concrete aggregate type into an LLVM
  aggregate of component handles (`{ptr, ptr, ...}`); construct / extract / update become native
  `insertvalue` / `extractvalue`; the components stay opaque handles, so a component's own
  operations (copy, equality, arithmetic) remain runtime calls, and codegen unrolls each
  whole-aggregate operation per type. This monomorphizes the aggregate's _shape_, not its element
  _values_.
- **Physical value monomorphization.** Component bytes live inline in the aggregate (a packed
  value's or string's storage physically inside the struct); every operation is native. This
  reproduces the value layer's physical layout and copy semantics in generated code -- the fully
  C++-symmetric endpoint, and the largest one.

Structural monomorphization is a hybrid, not the native endpoint: it removes the structural runtime
calls but keeps opaque leaf handles, so most of the runtime-value lifetime remains. The clean
endpoints are erasure (now) and physical value monomorphization (a later, whole-value-model
project).

## Decision

On the opaque-handle execution baseline, **every aggregate value is realized by erasure -- a
runtime-owned opaque value object reached through one handle.** A fixed-arity aggregate is not
structurally monomorphized; that is deferred until the backend commits to a physical value layout
for the whole value model, at which point the native path is adopted uniformly (scalars included),
not for aggregates alone.

The LIR aggregate operations -- `AggregateExtractInstr`, `AggregateUpdateInstr`, and the
`AggregateSelector` they carry -- are realization-agnostic logical value operations. This decision
is entirely below LIR, in LIR-to-LLVM codegen; LIR does not change with it, and a future physical
layout changes only the codegen realization of the same LIR operations.

`value::RuntimeTuple` and `value::RuntimeValue` are the aggregate members of the opaque-handle value
model, not a temporary tuple mechanism. A later physical layout would make them an optimization
alternative, not retroactively make the erased object wrong.

## Why erasure, not structural monomorphization

- **Variable-size containers must be erased regardless.** A dynamic array, a queue, and an
  associative array have a runtime element count (and, for the associative array, a runtime key
  set); an LLVM aggregate cannot hold a runtime-sized collection, so these are runtime-owned objects
  behind a handle no matter what. Monomorphizing the fixed-arity aggregates would not remove the
  erased family; it would split it -- native structs beside erased queues -- and split the one
  realization shape the family otherwise shares.
- **The hybrid complicates cross-suspension lifetime.** An erased aggregate crosses a suspension as
  one handle to one activation-frame value; the components ride inside it. A `{ptr, ptr, ...}` of
  component handles pointing into the per-stretch scope would dangle after resume, so every
  component handle would need independent promotion. That fractures the lifetime model -- the
  transient scope and the activation frame -- back into per-component bookkeeping, the opposite of
  keeping one value's storage in one place.
- **The benefit is partial.** Structural monomorphization removes the construct / extract / update
  runtime calls but leaves each leaf operation a runtime call, and leaf operations dominate. The
  full native benefit needs physical value monomorphization; the hybrid pays lifetime complexity for
  a bounded win.

## Rejected alternatives

- **Structural monomorphization for fixed-arity aggregates now.** An awkward middle: it splits the
  aggregate family from the variable-size containers that must stay erased, fractures
  cross-suspension lifetime into per-component handles, and still leaves leaf operations as runtime
  calls. Reconsidered only with a measured performance reason and a broader native value-layout
  plan, never as a step taken because it "looks closer to native".
- **Physical value monomorphization now.** The fully native endpoint, but a large project that
  reproduces the value layer's physical layout in generated code for every value type. It is a
  value- model-wide optimization, out of scope for realizing aggregates correctly, and gated behind
  the correctness baseline being broad ([jit-value-realization](jit-value-realization.md)).

## Consequences

- The aggregate family shares one realization shape: a runtime-owned opaque value object. A new
  aggregate (a container) is realized the same way, with no per-type realization debate.
- Aggregate operations are runtime-library calls; a struct field access is a call, not a native
  `extractvalue`. This is the accepted baseline cost, the aggregate counterpart of every scalar
  operation being a runtime call on the execution backend.
- The physical-layout path, if pursued, is adopted for the whole value model and swaps the codegen
  realization of the unchanged LIR aggregate operations; it does not begin as a per-aggregate
  hybrid.

## Cross-references

- [jit-value-realization](jit-value-realization.md) -- the opaque-handle baseline and the deferred
  physical in-frame layout this extends to aggregates.
- [integral-representation](integral-representation.md) -- the runtime-fat scalar class both
  backends share, the pure-reuse case aggregates contrast with.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- the struct is MIR's product
  type (`TupleType`); this decides how that product is realized on the execution backend.
- `../architecture/lir.md` -- `AggregateExtractInstr` / `AggregateUpdateInstr` are logical value
  operations; physical realization is below LIR.
