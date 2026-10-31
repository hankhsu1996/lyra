# Value-type concepts (composable LRM operator contracts)

Date: 2026-06-17 Status: accepted. Revised 2026-10-30: `LyraValue` asks only what every holder of a
value asks, and `==`, `!=` and the unknown-bit test left it.

## Context

The runtime layer `lyra::value::*` realises every SystemVerilog data type as a host C++ type
(`PackedArray`, `String`, `UnpackedArray<T>`, `DynamicArray<T>`, `Queue<T>`,
`AssociativeArray<K,V>`, `Real`). Two adjacent concerns reach into these types and have been
entangled:

1. **LRM operator coverage.** LRM 11.4.5 defines `==`/`!=` for any data type, `===`/`!==` for any
   data type except `real`/`shortreal`, `==?`/`!=?` for integral only, relational `<`/`<=`/`>`/`>=`
   for integral / real / string. Each value type's surface must reflect what LRM defines on it. The
   existing surface was grown incrementally and is uneven: `PackedArray` has everything;
   `AssociativeArray` has none of the equality family; `Queue` has only a host-bool case-equality
   predicate and no `==`/`!=` or PackedArray-returning `===`; `String::operator==` returns `bool`
   while every other container's `==` returns `PackedArray`.
2. **Runtime change-detection hook.** `lyra::runtime::Var<T>::Set` (LRM 4.3 / 9.4.2 update event)
   asks one question per write: "did the cell's bit-pattern change". For integral signals this has
   been computed by `PackedArray::IsCaseEqual` -- the same algorithm as the SV `===` operator. The
   shared algorithm collapsed two distinct roles (engine hook vs LRM operator) into one method pair
   (`IsCaseEqual` / `CaseEqual`), with the name borrowing LRM terminology for what is, at the
   `Var<T>` callsite, an engine-internal predicate.

The entanglement surfaces as two visible smells:

- `IsCaseEqual` (host bool) and `CaseEqual` (PackedArray) look like a "two output forms of one
  operation" pair, but they answer two different questions; the name pairing hides that.
- The C++ backend's `IsObservableScalarType(mir::Type)` predicate gates wrap-in-`Var<T>` and the
  Set/Get/Mutate dispatch by checking whether the value type is integral-packed (plus `ExternalRef`
  as a sibling intrinsic-observable). This is a transitional approximation -- the LRM-correct gate
  is "is this storage a module-level cell", not "is its value type integral" -- and it leaks because
  the value-type layer never declared which types are eligible for observable wrapping.

A planned refactor will lift the observable wrapping into MIR's type system as a first-class
`ObservableType`. That cleanup is blocked on a clean answer to "which value types are eligible to be
wrapped". The answer cannot be a predicate the backend recomputes -- it must be a contract the value
types declare and the type system enforces.

## Decision

**The `lyra::value::*` surface is structured as a small lattice of composable C++ concepts, each
mirroring one LRM operator family.** Every value type declares the concepts it satisfies; LRM Table
11-1 ("operators and data types") dictates which concepts a given type must satisfy and which it
must not. The runtime gates wrapping in `Var<T>` and `ObservableType{T}` on the most fundamental
concept; LRM-specific lowering paths gate on the relevant per-family concept.

### Concept lattice

```
LyraValue                -- what every value a holder keeps answers
  Storable               -- `std::copyable`, container-safe relocation
  IsBitIdentical(T) const -> bool      // engine change-detection hook
                                       // (LRM 9.4.2 update event predicate)

CaseEqualComparable : LyraValue
  CaseEqual(T) const -> PackedArray    // LRM 11.4.5 === (Any; a real compares values)
                                       // by convention shares internal impl with IsBitIdentical

WildcardComparable : LyraValue
  WildcardEquals(T) const -> PackedArray  // LRM 11.4.5 ==? (Integral only)

Ordered : LyraValue
  operator< / <= / > / >= -> PackedArray  // LRM 11.4.4 relational (Integral / Real / String)
```

The four concepts are independent refinements of `LyraValue`. Each value type opts in to the subset
LRM defines for it. Every typed C++ value type -- each the table below lists -- also states `==`,
`!=` and whether it holds an unknown bit as members of its own (LRM 11.4.5, 20.9), but `LyraValue`
does not ask them: a value the library holds of a type it was compiled without is compared and
streamed through the operations handed to whatever asks
([a-type-is-told-to-the-library-as-a-record-both-sides-read](a-type-is-told-to-the-library-as-a-record-both-sides-read.md)
D4), so the concept asks only what every holder asks.

### Per-type concept membership

| Type                    | `LyraValue` | `CaseEqualComparable` | `WildcardComparable` | `Ordered`                |
| ----------------------- | ----------- | --------------------- | -------------------- | ------------------------ |
| `PackedArray`           | yes         | yes                   | yes                  | yes                      |
| `String`                | yes         | yes                   | no (LRM excludes)    | yes                      |
| `UnpackedArray<T>`      | yes         | yes                   | no                   | no (LRM does not define) |
| `DynamicArray<T>`       | yes         | yes                   | no                   | no                       |
| `Queue<T>`              | yes         | yes                   | no                   | no                       |
| `AssociativeArray<K,V>` | yes         | yes                   | no                   | no                       |
| `lyra::value::Real`     | yes         | yes (compares values) | no                   | yes                      |

An enum has no row: an enum value is not a distinct runtime type but a `PackedArray` at its base
integral shape (`enum-representation.md`; `integral-representation.md` invariant 1 -- one C++ class
for every integral, and slang models `EnumType` as an `IntegralType`). Its nominal content (member
list, base) is consumed at HIR-to-MIR -- `first/last/num` fold to constants and `name/next/prev`
become questions put to the member list the unit states -- so nothing in the value-concept lattice
is enum-specific.

`Real`'s row is the one the standard leaves open. LRM Table 11-1 gives `===` / `!==` no real
operand, and case equality's bit-by-bit match (LRM 12.5) reads a real two ways: a bit-pattern match
makes `+0.0 === -0.0` false and `NaN === NaN` true, a value match the reverse. The front end accepts
the operator on a real and evaluates it as `==`, and Lyra keeps that reading, so `Real::CaseEqual`
compares values while `IsBitIdentical` compares bit patterns (revised 2026-10-16; until then
lowering refused the form and `Real` had no `CaseEqual`).

### Universal-equality return type

`operator==` and `operator!=` return `PackedArray` uniformly across every value type. For inherently
2-state types (`String`, future `Real`), the result is a 1-bit 2-state `PackedArray` that is always
`0` or `1`; for 4-state-aware types it may be `x`. The uniform return type lets the lowering and
emit paths treat every `==`/`!=` callsite identically -- no per-type return-shape branching. The
slight cost (constructing a 1-bit `PackedArray` for a fundamentally 2-state result) is paid in the
runtime; the entire compiler-and-emit pipeline above it stays uniform.

### `IsBitIdentical` vs `CaseEqual`

These are separate methods on a value type even when they share an internal algorithm:

- `IsBitIdentical(T) const -> bool` is a member of `LyraValue`. It is the engine's change-detection
  question ("did the cell's bit-pattern change between two writes"). Its name reflects its role, not
  an LRM operator. It returns the truthful host-bool result.
- `CaseEqual(T) const -> PackedArray` is a member of `CaseEqualComparable`. It is the LRM `===`
  operator's value-type realisation. The result type carries the SV-typed 1-bit packed value that
  participates in further SV expression evaluation.

For integral values the two methods share an internal helper (bit-by-bit identity check), because
for them case equality _is_ bit identity. For `Real` they differ. `IsBitIdentical` compares bit
patterns via `std::bit_cast<std::uint64_t>(double)`, so that `+0.0` and `-0.0` are distinct and NaN
bit-patterns classify by identity, both required for "did the storage cell's bits change", while
`Real::CaseEqual` compares values.

### `Var<T>` and `ObservableType{T}` gating

`Var<T>` requires only `LyraValue`. Every type that can be wrapped as observable storage satisfies
this concept, no more.

MIR's `ObservableType{T_mir}` wraps an MIR value type to declare module-scope observable storage.
HIR-to-MIR's wrap rule is structural -- a structural-var declaration whose value type is a "data
storage" kind is wrapped; pointer / vector / object-handle / external-ref / event / chandle / void /
scope / self types are not. The C++ emit then sees `ObservableType{T_mir}` and renders `Var<T_cpp>`.
The C++ template instantiation `Var<T_cpp>` requires `LyraValue<T_cpp>`; a value type that forgot to
implement the contract triggers a compile-time concept failure at the instantiation site. The MIR
type system gates "this storage is observable"; the C++ concept system gates "this value type can
fulfill the observable contract". Both gates compose without duplicating the predicate.

The old backend predicate `IsObservableScalarType(mir::Type)` is removed.

### Lowering observable reads and writes

Superseded by [storage-access-as-place-formation](storage-access-as-place-formation.md), and that
one in part by [owner-transition-and-observation](owner-transition-and-observation.md): naming the
storage a wrapper represents is place formation rather than a call, while reading what it holds and
replacing the whole of it are calls after all. The rest of this entry stands. Of the lowering shapes
below, the read and the whole write survive that round trip minus the engine handle they carry here,
which is ambient; everything the shapes say about a partial write does not.

An observable structural variable's read and write are SV-level implicit operations on the variable;
the C++ realisation of these operations is a method call on the `Var<T>` wrapper (`Get` / `Set` /
`Mutate`). Per `mir.md` invariant 10, the fact that the operation is a method call must be explicit
in MIR; the C++ backend never re-decides it, and neither does the LLVM-via-LIR path. HIR-to-MIR
therefore lowers an observable read / write into an ordinary `CallExpr` whose receiver is the
`MemberAccess` reaching the cell and whose argument vector includes the engine handle when the
wrapper method needs it (`Set` / `Mutate`):

```
SV: x = sig;        // observable read
MIR: ... = Call(BuiltinFnCallee{kGet}, [MemberAccess(self, sig)])

SV: sig = v;        // observable whole-write
MIR: ExprStmt(Call(BuiltinFnCallee{kSet}, [
       MemberAccess(self, sig),
       Call(BuiltinFnCallee{kServices}, [self]),
       v
     ]))

SV: sig.itoa(42);   // mutating method on observable receiver
MIR: ExprStmt(Call(BuiltinFnCallee{kItoa}, [
       Call(BuiltinFnCallee{kMutate}, [
         MemberAccess(self, sig),
         Call(BuiltinFnCallee{kServices}, [self])
       ]),
       42
     ]))
```

This follows the same architectural pattern `runtime-effects-as-generic-calls.md` records for
`$display` / `$finish` / file IO: every runtime operation is an ordinary `CallExpr`; the engine
handle is one argument among others; MIR does not know which argument plays which role. The pattern
applies here without modification because an observable cell's API is, at the C++ realisation level,
just another runtime library API.

### `lyra::value::Real` / `ShortReal` / `RealTime`

`Real` / `ShortReal` wrap `double` / `float` and satisfy `LyraValue`, `CaseEqualComparable` and
`Ordered`, and their `===` compares values. `RealTime` is the same C++ type as `Real` per LRM
6.12.1. The runtime exposes the full LRM operator surface -- arithmetic (`+` / `-` / `*` / `/` /
`**`), unary negation, relational (`<` / `<=` / `>` / `>=` returning a 1-bit `PackedArray`),
equality (`==` / `!=` returning 1-bit `PackedArray`), `IsBitIdentical` via
`std::bit_cast<std::uint64_t>(double)` so `+0.0` / `-0.0` and NaN bit-patterns classify by identity
(required for LRM 9.4.2 update-event detection), and the SV `int<->real` conversions. HIR-to-MIR's
observable wrap rule treats `RealType` / `ShortRealType` like any other value-storage type (a
`realtime` is a `real` there, LRM 6.12); a module-level `real` signal is observable through the same
mechanism as a module-level `int` signal.

## Forbidden shapes

- A method on `lyra::value::*` whose name borrows an LRM operator's terminology while serving the
  runtime engine's change-detection role. The engine hook is `IsBitIdentical`; the LRM `===`
  realisation is `CaseEqual`. They are separate even when their internal algorithm coincides.
- A backend predicate that decides "is this storage observable" by inspecting the value type's
  variant kind (the removed `IsObservableScalarType` shape). Observability is an MIR-type-system
  fact (`ObservableType` wrapping); eligibility to be wrapped is a C++ concept fact (`LyraValue`).
  The backend reads one or the other; it does not synthesise either.
- A value type whose `CaseEqual` and `IsBitIdentical` give different answers where its `===` is bit
  identity, which is every type but the real family. A real's `===` compares values, so there the
  two differ on signed zeros and NaN.
- A value type that implements an LRM operator method (e.g., `WildcardEquals`) without declaring the
  corresponding concept membership. The concept system is the contract; the methods exist to satisfy
  the concept.
- A new MIR node kind invented to express observable read / write (a `ReadCell` / `WriteCell` /
  `MutateCell` node). Observable operations lower to ordinary `CallExpr` against the `Var<T>`
  wrapper API; this matches `mir.md`'s forbidden shape against inventing MIR node kinds for
  backend-side storage realisations and the pattern recorded in
  `runtime-effects-as-generic-calls.md`.
- A backend predicate that decides whether a read should append `.Get()`, whether a write should
  route through `.Set()`, or whether a method receiver should switch between `.` and `->`. The
  decision belongs in HIR-to-MIR, expressed as the `CallExpr` shapes above. A predicate of this
  shape at render time is the inverse of `mir.md` invariant 10.

## Notes

The decision encodes LRM Table 11-1's "operators and data types" structure directly in the C++ type
system. Each row of Table 11-1 corresponds to one concept; each operand-type cell determines which
types satisfy it. Adding LRM operator families later (arithmetic, bitwise, shifts, reductions, ...)
follows the same shape -- one concept per family, types opt in to the ones LRM defines on them.
