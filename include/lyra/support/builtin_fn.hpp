#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// Closed namespace of compiler-recognized runtime entries. Same identity at
// HIR and MIR; lives in the support layer so neither layer's vocabulary
// imports the other's. The receiver type at the call site names the runtime
// library type whose method is being invoked (value-layer containers,
// observable storage cells, runtime services, scope handle); this enum
// carries only the method identity. See
// `docs/decisions/builtin-call-identity.md`.
enum class BuiltinFn : std::uint16_t {
  // LRM 7.4 / 7.8 / 7.10 / 11.5 positional access. Bare returns value
  // form; `Ref` returns write-through reference. `Slice` is read,
  // `SliceRef` is write.
  kElement,
  kElementRef,
  kSlice,
  kSliceRef,
  // LRM 7.4.3 / 7.5 / 7.9 / 7.10.2. AA's `num` is an alias of `size`;
  // String's LRM 6.16.1 `len` is its own mandated spelling.
  kSize,
  kLen,
  // LRM 7.12 / 7.5 / 7.10 container ops.
  kToOwned,
  kDelete,
  // LRM 7.12 ordering. `Sort` / `Rsort` take a `with`-clause closure as
  // the second argument.
  kReverse,
  kSort,
  kRsort,
  // LRM 7.12.3 reductions. All take a `with`-clause closure.
  kSum,
  kProduct,
  kAnd,
  kOr,
  kXor,
  // LRM 7.12.1 search.
  kFind,
  kFindIndex,
  kFindFirst,
  kFindFirstIndex,
  kFindLast,
  kFindLastIndex,
  kMin,
  kMax,
  kUnique,
  kUniqueIndex,
  // LRM 7.12.5.
  kMap,
  // LRM 7.10 queue insertion / removal.
  kInsert,
  kPopFront,
  kPopBack,
  kPushFront,
  kPushBack,
  // LRM 7.9. The traversal family (LRM 7.9.4 -- 7.9.7) writes the
  // visited key through a `ref` index argument and returns 0 / 1 / -1.
  kExists,
  kAssocFirst,
  kAssocLast,
  kAssocNext,
  kAssocPrev,
  // LRM 6.16 string methods.
  kGetc,
  kPutc,
  kToupper,
  kTolower,
  kCompare,
  kIcompare,
  kSubstr,
  // LRM 6.16.9 -- 6.16.13 string parse.
  kAtoi,
  kAtohex,
  kAtooct,
  kAtobin,
  kAtoreal,
  // LRM 6.16.14 -- 6.16.18 string format. Mutates the receiver.
  kItoa,
  kHextoa,
  kOcttoa,
  kBintoa,
  kRealtoa,
  // LRM 15.5 named-event operations. `Trigger` / `Await` arise from
  // `-> e;` / `@e;` collapsing at HIR -> MIR.
  kTrigger,
  kAwait,
  kTriggered,
  // LRM 6.19.5 enum type-static queries. `constexpr` in the runtime so
  // downstream optimizers fold the call.
  kEnumFirst,
  kEnumLast,
  kEnumNum,
  // LRM 6.19.5 enum instance methods.
  kEnumName,
  kEnumNext,
  kEnumPrev,
  // LRM 20.9 / 21.3.4.3. 2-state packed types return false; downstream
  // constant-folds those calls.
  kIsUnknown,
  // Observable storage cell operations. `Set` / `Mutate` thread services
  // as the second argument.
  kGet,
  kSet,
  kMutate,
  kServices,
  // Engine submit operations. `SubmitNba` is a `RuntimeServices` method
  // taking a closure; `SubmitObserved` is a `Scope` method taking the
  // deferred-check site id and a closure (LRM 16.14.6 last-write-wins
  // settle).
  kSubmitNba,
  kSubmitObserved,
};

// True iff `id` is a type-namespace-qualified static call -- no receiver,
// the qualifier is part of the symbol identity (e.g. `MyEnum::first()`).
// Used by HIR-to-MIR to pick `BuiltinStaticCallee` vs `BuiltinFnCallee`
// without re-deriving the family axis.
[[nodiscard]] constexpr auto IsStaticBuiltinFn(BuiltinFn id) -> bool {
  return id == BuiltinFn::kEnumFirst || id == BuiltinFn::kEnumLast ||
         id == BuiltinFn::kEnumNum;
}

// True iff the function modifies its receiver argument's storage in place.
// Used to route observable cells through `Mutate` so the receiver is the
// commit snapshot, not a Get'd value.
[[nodiscard]] auto IsMutatingBuiltinFn(BuiltinFn id) -> bool;

// True iff the call's `args[0]` is the container being indexed or sliced
// (rather than a value receiver). Used by LHS-chain walkers to reach the
// root primary.
[[nodiscard]] auto IsContainerAccessFn(BuiltinFn id) -> bool;

// True iff the LRM 7.12 method takes a `with`-clause closure as its second
// argument. The other LRM 7.5 / 7.10 array entries (`size`, `delete`,
// `reverse`) take no closure.
[[nodiscard]] auto ArrayMethodTakesClosure(BuiltinFn id) -> bool;

// True iff `id` is an associative-array traversal entry (LRM 7.9.4 -- 7.9.7).
// The traversal family lowers to an immediately-invoked closure (mutates the
// index argument and runs the write-back inline).
[[nodiscard]] auto IsAssociativeTraversalFn(BuiltinFn id) -> bool;

// Short snake-case name for the id. Used for HIR / MIR dumps and any
// diagnostic that names the runtime entry; aligns with the SV method
// spelling where one exists (LRM 6.16 / 7.9 / 7.12 / 6.19.5) and a
// descriptive name where there is no SV-side surface (`get` / `set` /
// `mutate` / `services`).
[[nodiscard]] auto BuiltinFnName(BuiltinFn id) -> std::string_view;

}  // namespace lyra::support
