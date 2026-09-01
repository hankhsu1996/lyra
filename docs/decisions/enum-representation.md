# Enum: nominal semantic type, base-integral value representation

Date: 2026-07-27 Status: accepted

## Context

An enum has two aspects that must not be conflated: its **semantic type identity** and its **runtime
value representation**. Lyra previously modeled an enum value as a distinct C++ type,
`Enum<Derived> : public PackedArray`. That distinct value type is a phantom -- it adds no storage
over its base integral -- and it is the root of a class of bugs: every generic runtime site that
branches on `same_as<T, PackedArray>` (`Var<T>`, `Net<T>`, `ValueStorageCore<T>`, the variadic
`Concat` constraint, associative-array key equality) silently routes an enum value to the "not a
packed array" branch, because an `Enum<Derived>` is a _foreign_ C++ type there. It also contradicts
`integral-representation.md` (one C++ class for every integral), since slang models `EnumType` as an
`IntegralType`.

A codebase audit established the true shape of the enum in Lyra today:

- `mir::EnumType { base, members }` and `lir::EnumType { base, members }` are distinct type variants
  that persist HIR -> MIR -> LIR -> backend, by design: an enumeration keeps a type of its own, and
  a value operation reads it through its base's packed shape.
- Lyra's general type-system rule (`mir-type-interning.md`) is that **nominal semantic types retain
  identity even when two types share a runtime representation**; struct (`struct_id`) and class
  (`class_id`) already follow it, and enum is keyed on its enumerator set as its declaration
  identity (LRM 6.19).
- Every _value_ operation -- assignment, cast, comparison, `case`, `$display`, DPI, function
  argument/return, observable wrapping -- already runs on the base integral, reached through the
  packed-shape query. The enum type identity is consumed downstream of HIR-to-MIR by exactly one
  thing: the six LRM 6.19.5 methods (`first/last/num/name/next/prev`), which read the member table.

So the mistake was never "MIR keeps `EnumType`." The mistake was the false implication
`distinct semantic type => distinct C++ runtime value class`.

## Decision

**A semantic type may retain nominal identity through MIR/LIR without owning a distinct runtime
value representation.** This is a general compiler principle; the enum is the current important
example. For enum specifically:

> `EnumType` is a nominal semantic type whose value-domain projection is its base packed integral
> type.

Concretely:

1. **Keep the semantic type.** `mir::EnumType` and `lir::EnumType` remain. `opcode_e` and
   `logic [6:0]` may share an execution representation while remaining different semantic types in
   MIR/LIR -- the same way a struct and a class do. Do not erase `EnumType`, and do not make enum an
   enum-specific exception to the nominal-identity rule.

2. **Erase the value representation to the base integral.** There is exactly one runtime
   representation for a packed integral value: `PackedArray`. An enum value adds no per-value state.
   An enum-typed value/storage/signal is realized as its base -- `PackedArray`, `Var<PackedArray>`,
   `Net<PackedArray>` -- never as a distinct C++ value species. The enumerator table (name, value,
   declaration order) belongs to the enum **type**, not to each value. The backend's value renderer
   projects an enum type to `PackedArray` through the existing integral projection; it never asks
   "is this value an `EnumType`?" to choose a runtime representation.

3. **Enum methods are type-owned operations, not value-object methods.** The SV surface syntax
   `e.name()` / `e.next(k)` does not imply an object with methods. Their semantic form is
   `name(enum-type, value)` / `next(enum-type, value, step)` -- the enum type supplies compile-time
   metadata; the runtime operands are ordinary values. `first/last/num` are type-level operations
   with no runtime value input.

4. **The six methods are realized as enum-associated program artifacts, resolved above the
   backend.** `first/last/num` lower to ordinary constants at HIR-to-MIR (a member-value constant, a
   member-value constant, a compile-time integral count). The nontrivial `name/next/prev` lower to
   **ordinary MIR callables synthesized at HIR-to-MIR**, whose bodies are expressed entirely in
   generic MIR primitives (comparison, arithmetic, conditional control flow, integer/string
   literals) encoding the LRM 6.19.5 algorithm over the member table. A call site is an ordinary
   `CallExpr` referencing that callable. Because these are ordinary program callables -- MIR
   callable -> LIR CFG -> emitted function -- the optimizer can inline, constant-propagate,
   eliminate dead arms, and synthesize jump tables, exactly as for any user function. This is
   fundamentally different from a linked runtime-library helper, which is opaque to that
   optimization.

5. **Ownership follows the existing per-unit model.** Enum types are already interned and
   materialized per using compilation unit. The enum-associated synthesized callables live in the
   same unit as internal program artifacts; a module using `p::opcode_e` gets its own synthesized
   implementation associated with its locally interned `EnumType`. Do not expose hidden enum helpers
   through a defining package's interface to deduplicate them -- that degrades the compilation-unit
   interface model to solve a code-size detail. If duplication later becomes measurable, solve
   artifact deduplication as a general unit/link-time problem, not an enum-specific exception.

`semantic result type != runtime carrier type` is a normal concept and must remain usable: a method
result may carry `EnumType` as its MIR type while its runtime representation is the base
`PackedArray`.

## Consequences

- The `same_as<T, PackedArray>` class of bugs dissolves at the root: enum values are `PackedArray`,
  so no generic runtime template ever sees a foreign enum C++ type. Do NOT fix those bugs by
  teaching runtime templates that `Enum<Derived>` is "also integral" -- that preserves the wrong
  representation distinction.
- `integral-representation.md` invariant 1 (one C++ class per integral) is restored for enums.
- `EnumType` remains legitimate for type checking, identity, dump/debug output, the enum-associated
  semantic lowering above, and later type-aware transformations.
- The value-type concept lattice has no enum row: an enum is not a distinct runtime value type (see
  `value-type-concepts.md`).

## Forbidden shapes

- A distinct C++ runtime value class for an enum (`Enum<Derived> : public PackedArray`), or an enum
  value flowing through generic runtime code as a non-`PackedArray` `T`.
- A backend value renderer that inspects a value's type to decide its runtime representation is
  `EnumType` (the projection to base is already explicit through the integral machinery).
- A backend render that reads a call's receiver/qualification type to discover which enum it came
  from, find a descriptor, and synthesize a different call. The call site must already name the
  intended operation/artifact before rendering (`backend_contract.md`).
- An opaque runtime-library helper as the semantic implementation of an enum method
  (`EnumName(value, table)`, `EnumNext(value, table, step)`). That moves enum lowering into the
  library and hides it from LIR/LLVM optimization (`mir.md`).
- Encoding an enum method as an opaque MIR operation merely to make codegen convenient. The
  operation lowers to generic MIR primitives / an ordinary synthesized callable.
- Dropping `mir::EnumType` / `lir::EnumType` because the value carrier is `PackedArray`. The nominal
  type identity is separate from the value representation and is retained.
- A new cross-unit enum mechanism, or exposing synthesized enum helpers through a package interface
  to deduplicate them.

## Terminal model

```
EnumType        = nominal semantic type (kept in MIR/LIR)
enum value      = base PackedArray representation
enum member table = type metadata
enum methods    = compiler-resolved, type-owned semantics, realized as
                  generic program computation (constants for first/last/num;
                  synthesized MIR callables for name/next/prev)
```

This separation is the terminal model, not an intermediate migration shape.
