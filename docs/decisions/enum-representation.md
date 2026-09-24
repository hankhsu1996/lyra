# Enum: nominal semantic type, base-integral value representation

Date: 2026-07-27 Status: accepted; point 4 revised 2026-09-24 (see the last section)

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

A codebase audit at the time established the shape of the enum in Lyra:

- `mir::EnumType { base, members }` and `lir::EnumType { base, members }` were distinct type
  variants that persisted HIR -> MIR -> LIR -> backend: an enumeration kept a type of its own, and a
  value operation read it through its base's packed shape.
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

1. **Keep the semantic type.** `mir::EnumType` remains. `opcode_e` and `logic [6:0]` may share an
   execution representation while remaining different semantic types in MIR -- the same way a struct
   and a class do. Do not erase `EnumType` from MIR, and do not make enum an enum-specific exception
   to the nominal-identity rule. Below MIR every question that reads the member list has already
   been stated as a call on it, so nothing there tells an enumeration from its base and LIR carries
   the base.

2. **Erase the value representation to the base integral.** There is exactly one runtime
   representation for a packed integral value: `PackedArray`. An enum value adds no per-value state.
   An enum-typed value/storage/signal is realized as its base -- `PackedArray`, `Var<PackedArray>`,
   `Net<PackedArray>` -- never as a distinct C++ value species. The member list (name, value,
   declaration order) belongs to the enum **type**, not to each value. The backend's value renderer
   projects an enum type to `PackedArray` through the existing integral projection; it never asks
   "is this value an `EnumType`?" to choose a runtime representation.

3. **Enum methods are type-owned operations, not value-object methods.** The SV surface syntax
   `e.name()` / `e.next(k)` does not imply an object with methods. Their semantic form is
   `name(enum-type, value)` / `next(enum-type, value, step)` -- the enum type supplies compile-time
   metadata; the runtime operands are ordinary values. `first/last/num` are type-level operations
   with no runtime value input.

4. **The member list is data the unit states once; the questions asked of it are one library routine
   each.** `first/last/num` lower to ordinary constants at HIR-to-MIR (the first member's whole
   value, the last member's, the member count). `name`, `next(N)`, `prev(N)`, and whether a value is
   a member (LRM 6.24.2) are calls whose receiver is the enumeration's member list -- a description
   the unit holds beside a type's other run-time descriptions, built from every member's whole value
   and name in declared order -- and whose operands are the value and the step. The search and the
   wrap are written once, in the runtime library, and are the same for every enumeration. See
   "Revised 2026-09-24" below for why this replaced synthesized callables.

5. **Ownership follows the existing per-unit model.** Enum types are already interned and
   materialized per using compilation unit, and so is the member list a unit states for one; a
   module using `p::opcode_e` states its own. Do not expose it through a defining package's
   interface to deduplicate it -- that degrades the compilation-unit interface model to solve a size
   detail.

6. **A member is its whole value.** Over a 4-state base a member may hold x or z bits, and the base
   may be wider than a machine word (LRM 6.19), so MIR carries each member as the full constant at
   the base type, both planes, and a value is a member when it is bit-identical to one.

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
- A backend render that reads the type a call is made at to discover which enum it came from, find a
  descriptor, and synthesize a different call. The call site must already name the intended
  operation/artifact before rendering (`backend_contract.md`).
- Code per member in anything the compiler states for an enumeration. It fails iteration time (north
  star 1): the release build of a real core spent most of its CPU time on it.
- Dropping `mir::EnumType` because the value carrier is `PackedArray`. The nominal type identity is
  separate from the value representation and is retained where something reads it; below MIR nothing
  does, so LIR carries an enumeration as its base.
- A new cross-unit enum mechanism, or exposing synthesized enum helpers through a package interface
  to deduplicate them.

## Terminal model

```
EnumType          = nominal semantic type (kept in MIR; LIR carries its base)
enum value        = base PackedArray representation
enum member list  = unit-held description: every member's whole value and name
enum methods      = constants for first/last/num; a question put to the
                    member list for name/next/prev and membership
```

This separation is the terminal model, not an intermediate migration shape.

## Revised 2026-09-24: the questions are the library's, not code per enumeration

Point 4 used to synthesize a MIR callable per enumeration for `name`, `next` / `prev`, and
membership, each a conditional nested one level per member. Its stated reason: an ordinary program
callable lets the optimizer "inline, constant-propagate, eliminate dead arms, and synthesize jump
tables", which a linked library helper hides. That requirement is real and narrower than the set
this decision now answers to:

- **The optimizer is the cost, not the remedy.** Ibex's `--release` build spent 540 of 888 CPU
  seconds in InstCombine on these bodies, the worst of them called from nowhere. One 200-member
  enumeration did not finish a `--release` build in ten minutes; at 300 members the chain exceeded
  clang's bracket depth. With the member list as data, the same 200-member design builds in 4.9 s on
  the C++ backend and 0.8 s on the execution backend.
- **The default build optimizes nothing.** Design code is compiled unoptimized, so the per-call work
  ran unoptimized as well; the library is prebuilt and always optimized.
- **Traversing a set in declared order already has one shape.** An associative array's `first` /
  `next` / `prev` (LRM 7.9) are library operations over data whose meaning the operation fixes. A
  per-enumeration loop over a constant table would have met the size requirement too, and was
  rejected for being a second shape for the same thing and for running unoptimized.
- **A call whose value is a compile-time constant is rare**, and the three methods that take no
  value stay constants.

How the field does it: Verilator (`V3Width.cpp`, `enumVarp` / `enumSelect`) builds constant tables
indexed by value, which only a 2-state simulator can -- a value holding x indexes nothing, so that
answer is out here. slang (`EnumMethods.cpp`, `EnumNextPrevMethod::eval`) collects the members,
finds the value's position, and steps the position modulo the count in one generic routine, with
only the member list per type. This decision is slang's split.

The rewrite also found three wrong answers the old shape gave on both backends: a member holding x
answered `name()` with the empty string, a member past 64 bits did the same (MIR kept a member as
its low 64 bits and no unknown plane), and `next` / `prev` with a step past 2^31 stepped the wrong
way (the step was carried signed and `prev` negated it). Point 6 is what fixes the first two; the
step now reaches the library as the `int unsigned` the source passed.
