# Value-type concepts (composable LRM operator contracts)

Date: 2026-06-17 Status: accepted

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

`refactor.md` R12 plans to lift the observable wrapping into MIR's type system as a first-class
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
LyraValue                -- the SV value contract every runtime value type realises
  Storable               -- `std::copyable`, container-safe relocation
  operator==(T) const -> PackedArray   // LRM 11.4.5 == (Any data type)
  operator!=(T) const -> PackedArray   // LRM 11.4.5 != (Any data type)
  IsBitIdentical(T) const -> bool      // engine change-detection hook
                                       // (LRM 9.4.2 update event predicate)
  HasUnknown() const -> bool           // LRM 20.9 $isunknown predicate

CaseEqualComparable : LyraValue
  CaseEqual(T) const -> PackedArray    // LRM 11.4.5 === (Any except real/shortreal)
                                       // by convention shares internal impl with IsBitIdentical

WildcardComparable : LyraValue
  WildcardEquals(T) const -> PackedArray  // LRM 11.4.5 ==? (Integral only)

Ordered : LyraValue
  operator< / <= / > / >= -> PackedArray  // LRM 11.4.4 relational (Integral / Real / String)
```

The four concepts are independent refinements of `LyraValue`. Each value type opts in to the subset
LRM defines for it.

### Per-type concept membership

| Type                    | `LyraValue`           | `CaseEqualComparable`               | `WildcardComparable` | `Ordered`                |
| ----------------------- | --------------------- | ----------------------------------- | -------------------- | ------------------------ |
| `PackedArray`           | yes                   | yes                                 | yes                  | yes                      |
| `Enum<Derived>`         | yes (via inheritance) | yes                                 | yes                  | yes                      |
| `String`                | yes                   | yes                                 | no (LRM excludes)    | yes                      |
| `UnpackedArray<T>`      | yes                   | yes                                 | no                   | no (LRM does not define) |
| `DynamicArray<T>`       | yes                   | yes                                 | no                   | no                       |
| `Queue<T>`              | yes                   | yes                                 | no                   | no                       |
| `AssociativeArray<K,V>` | yes                   | yes                                 | no                   | no                       |
| `lyra::value::Real`     | yes                   | **no (LRM excludes real from ===)** | no                   | yes                      |

`Real`'s row is the load-bearing case: LRM Table 11-1 excludes `real` / `shortreal` from `===` /
`!==`, and the backend follows that strictly -- `Real` does not satisfy `CaseEqualComparable` and
has no `CaseEqual`. The exclusion is principled, not an oversight: LRM 12.5 defines case equality as
a bit-by-bit match, but a real has no x/z plane and its two natural readings disagree -- a
bit-pattern match makes `+0.0 === -0.0` false and `NaN === NaN` true, while a value match makes them
true and false respectively. The LRM declines to pick, so the operator is simply not defined on
real. (slang and Verilator accept `r1 === r2` and evaluate it as `r1 == r2`, a value comparison;
that is a documented deviation from Table 11-1 that the backend does not adopt.) Because the
frontend does not reject the form, HIR-to-MIR lowering does: a `===` / `!==` whose operand is real,
or an unpacked array with a real leaf, is an error (LRM Table 11-1). Nothing
real-case-equality-shaped reaches the backend, so the render path stays mechanical.

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

For types that satisfy both concepts the two methods share an internal helper (bit-by-bit identity
check), because for integral values case equality _is_ bit identity. `Real` satisfies only
`LyraValue`, so only `IsBitIdentical` exists -- it implements bit-pattern equality via
`std::bit_cast<std::uint64_t>(double)` so that `+0.0` and `-0.0` are distinct and NaN bit-patterns
classify by identity, both required for "did the storage cell's bits change". There is no
`Real::CaseEqual`: `===` / `!==` are not defined on real (Table 11-1), and lowering rejects the
source-level form before it could reach a value-type method.

### `Var<T>` and `ObservableType{T}` gating

`Var<T>` requires only `LyraValue`. Every type that can be wrapped as observable storage satisfies
this concept, no more.

MIR's `ObservableType{T_mir}` (R12) wraps an MIR value type to declare module-scope observable
storage. HIR-to-MIR's wrap rule is structural -- a structural-var declaration whose value type is a
"data storage" kind is wrapped; pointer / vector / object-handle / external-ref / event / chandle /
void / scope / self types are not. The C++ emit then sees `ObservableType{T_mir}` and renders
`Var<T_cpp>`. The C++ template instantiation `Var<T_cpp>` requires `LyraValue<T_cpp>`; a value type
that forgot to implement the contract triggers a compile-time concept failure at the instantiation
site. The MIR type system gates "this storage is observable"; the C++ concept system gates "this
value type can fulfill the observable contract". Both gates compose without duplicating the
predicate.

The old backend predicate `IsObservableScalarType(mir::Type)` is removed.

### Lowering observable reads and writes

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

`Real` / `ShortReal` wrap `double` / `float` and satisfy `LyraValue` + `Ordered`, not
`CaseEqualComparable` (LRM Table 11-1 excludes `real` / `shortreal` from `===`). `RealTime` is the
same C++ type as `Real` per LRM 6.12.1. The runtime exposes the full LRM operator surface --
arithmetic (`+` / `-` / `*` / `/` / `**`), unary negation, relational (`<` / `<=` / `>` / `>=`
returning a 1-bit `PackedArray`), equality (`==` / `!=` returning 1-bit `PackedArray`),
`IsBitIdentical` via `std::bit_cast<std::uint64_t>(double)` so `+0.0` / `-0.0` and NaN bit-patterns
classify by identity (required for LRM 9.4.2 update-event detection), and the SV `int<->real`
conversions. HIR-to-MIR's observable wrap rule treats `RealType` / `ShortRealType` / `RealTimeType`
like any other value-storage type; a module-level `real` signal is observable through the same
mechanism as a module-level `int` signal.

## Forbidden shapes

- A method on `lyra::value::*` whose name borrows an LRM operator's terminology while serving the
  runtime engine's change-detection role. The engine hook is `IsBitIdentical`; the LRM `===`
  realisation is `CaseEqual`. They are separate even when their internal algorithm coincides.
- A backend predicate that decides "is this storage observable" by inspecting the value type's
  variant kind (the removed `IsObservableScalarType` shape). Observability is an MIR-type-system
  fact (`ObservableType` wrapping); eligibility to be wrapped is a C++ concept fact (`LyraValue`).
  The backend reads one or the other; it does not synthesise either.
- A value type that satisfies `CaseEqualComparable` but whose `CaseEqual` and `IsBitIdentical` give
  different answers. The two methods may share an internal helper; they may not diverge. (`Real`
  sidesteps this by not satisfying `CaseEqualComparable` at all -- it has no `CaseEqual`, only the
  engine's bit-pattern `IsBitIdentical`.)
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
