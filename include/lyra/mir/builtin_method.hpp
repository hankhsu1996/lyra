#pragma once

#include <cstdint>
#include <variant>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// LRM 6.19.5 -- enum intrinsic methods.
enum class EnumMethodKind : std::uint8_t {
  kFirst,
  kLast,
  kNum,
  kNext,
  kPrev,
  kName,
};

// LRM 6.16.1 through 6.16.15 -- string intrinsic methods.
enum class StringMethodKind : std::uint8_t {
  kLen,
  kGetc,
  kPutc,
  kToupper,
  kTolower,
  kCompare,
  kIcompare,
  kSubstr,
  kAtoi,
  kAtohex,
  kAtooct,
  kAtobin,
  kAtoreal,
  kItoa,
  kHextoa,
  kOcttoa,
  kBintoa,
  kRealtoa,
};

// LRM 15.5 named event operations. Trigger is non-suspending; Await suspends
// the caller until the event fires; Triggered is a pure read of the
// slot-persistent triggered state.
enum class EventMethodKind : std::uint8_t {
  kTrigger,
  kAwait,
  kTriggered,
};

// Methods on the array-shaped containers (LRM 7.4 / 11.5 / 7.12). Bare
// vs `Ref` suffix encodes value-form vs reference-form access -- see
// `include/lyra/value/concepts.hpp` for the runtime API protocol. The LRM
// 7.12 `with`-clause family carries one closure as the second argument
// (the source `with` clause or the LRM 7.12.1 default `with (item)`).
enum class ArrayMethodKind : std::uint8_t {
  kElement,
  kElementRef,
  kSlice,
  kSliceRef,
  kToOwned,
  kSize,
  kDelete,
  kReverse,
  kSort,
  kRsort,
  kSum,
  kProduct,
  kAnd,
  kOr,
  kXor,
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
  kMap,
};

// LRM 7.10 queue-native methods plus the compiler-internal access methods
// that queue operators lower to. LRM 7.10.1 bakes the `q[$+1]` append
// semantic into the write-side element access; LRM 7.10 defines no
// write-side queue slice. The shared LRM 7.12 family stays in
// `ArrayMethodKind`.
enum class QueueMethodKind : std::uint8_t {
  kSize,
  kInsert,
  kDelete,
  kPopFront,
  kPopBack,
  kPushFront,
  kPushBack,
  kElement,
  kElementRef,
  kSlice,
};

// LRM 7.9 associative-array methods. LRM 7.8.6 / 7.8.7 split read from
// write because the AA allocates on the write-side path (the key is created
// with the element default before the write target is yielded). The
// traversal family (LRM 7.9.4 -- 7.9.7) assigns the visited key through a
// `ref` index argument and returns 0 / 1 / -1.
enum class AssociativeMethodKind : std::uint8_t {
  kElement,
  kElementRef,
  kNum,
  kSize,
  kExists,
  kDelete,
  kFirst,
  kLast,
  kNext,
  kPrev,
};

// Side-effect-free queries that are defined for every runtime value type
// (string, packed integral, unpacked array, ...). The receiver's MIR type
// alone determines the emit shape; lowering passes the operand uniformly.
// The LRM 21.3.4.3 "unknown bits in $sscanf str / format imply EOF" guard
// lowers to kIsUnknown; future LRM 20.9 `$isunknown` lowers to the same
// leaf.
enum class ValueMethodKind : std::uint8_t {
  kIsUnknown,
};

// LRM 7.12.4 iterator intrinsic methods (only `index` is in scope today).
enum class IteratorMethodKind : std::uint8_t {
  kIndex,
};

// Methods on the runtime scope handle (the `self` receiver). `kServices`
// reaches the engine facade `RuntimeServices` -- the engine handle every
// runtime-effect call threads as a plain argument
// (docs/decisions/runtime-effects-as-generic-calls.md).
enum class ScopeMethodKind : std::uint8_t {
  kServices,
};

// Operations on an observable storage cell (`ObservableType` /
// `ExternalRefType`). The cell is the first argument; `kSet` / `kMutate`
// thread the engine handle as the second. See
// docs/decisions/value-type-concepts.md.
enum class ObservableMethodKind : std::uint8_t {
  kGet,
  kSet,
  kMutate,
};

struct EnumMethodInfo {
  TypeId enum_type;
  EnumMethodKind kind;
};

struct StringMethodInfo {
  StringMethodKind kind;
};

struct EventMethodInfo {
  EventMethodKind kind;
};

struct ArrayMethodInfo {
  ArrayMethodKind kind;
};

struct QueueMethodInfo {
  QueueMethodKind kind;
};

struct AssociativeMethodInfo {
  AssociativeMethodKind kind;
};

struct ValueMethodInfo {
  ValueMethodKind kind;
};

struct IteratorMethodInfo {
  IteratorMethodKind kind;
};

struct ScopeMethodInfo {
  ScopeMethodKind kind;
};

struct ObservableMethodInfo {
  ObservableMethodKind kind;
};

struct BuiltinMethodCallee {
  std::variant<
      EnumMethodInfo, StringMethodInfo, EventMethodInfo, ArrayMethodInfo,
      QueueMethodInfo, AssociativeMethodInfo, ValueMethodInfo,
      IteratorMethodInfo, ScopeMethodInfo, ObservableMethodInfo>
      method;
};

// Whether the method mutates its receiver in place.
[[nodiscard]] auto IsMutatingBuiltinMethod(const BuiltinMethodCallee& callee)
    -> bool;

// Whether `callee` is an indexed / sliced container access whose first
// argument is the container being accessed. Used by LHS-chain walkers to
// reach the root primary.
[[nodiscard]] auto IsContainerAccessCall(const BuiltinMethodCallee& callee)
    -> bool;

// Closed namespace of compiler-internal runtime entries. Same identity
// shape as `support::SystemSubroutineId`; sections below mirror the
// protocol grouping in `value/concepts.hpp`. Resolution to a concrete
// C++ method or LLVM symbol is per-backend, keyed by `(BuiltinFn,
// receiver_type)` for `BuiltinFnCallee` and `(BuiltinFn, type_qual)` for
// `BuiltinStaticCallee`.
enum class BuiltinFn : std::uint16_t {
  // Indexable / SliceableRef (LRM 7.4 / 7.8 / 7.10 / 11.5). Universal
  // positional access -- bare returns value form, `Ref` returns
  // write-through reference. `Slice` is read; `SliceRef` is write.
  kElement,
  kElementRef,
  kSlice,
  kSliceRef,
  // Sized -- LRM 7.4.3 / 7.5 / 7.10.2 element-count query. Universal
  // across Dynamic / Unpacked / Queue / AA (AA's LRM 7.9 `num` is the
  // same symbol). `String::Len` (LRM 6.16.1) is a sibling under its own
  // mandated spelling.
  kSize,
  kLen,
  // Ownable / mutating container ops (LRM 7.12 + 7.5 / 7.10).
  kToOwned,
  kDelete,
  // Sortable (LRM 7.12). The closure-bearing siblings (`Sort` /
  // `Rsort`) take a `with`-clause closure as the second argument.
  kReverse,
  kSort,
  kRsort,
  // Reducible (LRM 7.12.3). All take a `with`-clause closure.
  kSum,
  kProduct,
  kAnd,
  kOr,
  kXor,
  // Searchable (LRM 7.12.1).
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
  // LRM 7.12.5 array projection.
  kMap,
  // Queue insertion / removal (LRM 7.10).
  kInsert,
  kPopFront,
  kPopBack,
  kPushFront,
  kPushBack,
  // Associative-array keyed queries (LRM 7.9). `Exists` is keyed
  // membership; the traversal family (LRM 7.9.4 -- 7.9.7) assigns the
  // visited key through a `ref` index argument and returns 0 / 1 / -1.
  kExists,
  kAssocFirst,
  kAssocLast,
  kAssocNext,
  kAssocPrev,
  // String character / case / compare / substring (LRM 6.16).
  kGetc,
  kPutc,
  kToupper,
  kTolower,
  kCompare,
  kIcompare,
  kSubstr,
  // String numeric parse (LRM 6.16.9 -- 6.16.13).
  kAtoi,
  kAtohex,
  kAtooct,
  kAtobin,
  kAtoreal,
  // String numeric format (LRM 6.16.14 -- 6.16.18). Mutate the receiver.
  kItoa,
  kHextoa,
  kOcttoa,
  kBintoa,
  kRealtoa,
  // Named-event operations (LRM 15.5). `Trigger` and `Await` arise from
  // `-> e;` / `@e;` collapsing at HIR -> MIR; `Triggered` surfaces from
  // the slang `e.triggered` method call.
  kTrigger,
  kAwait,
  kTriggered,
  // Enum type-static queries (LRM 6.19.5). Lowered as
  // `BuiltinStaticCallee` -- the enum type is the call's identity
  // qualifier, not a value receiver. `constexpr` in the runtime so
  // downstream optimizers fold.
  kEnumFirst,
  kEnumLast,
  kEnumNum,
  // Enum instance methods (LRM 6.19.5). Lowered as `BuiltinFnCallee` --
  // the enum value is the receiver at args[0].
  kEnumName,
  kEnumNext,
  kEnumPrev,
  // LRM 20.9 `$isunknown` plus the LRM 21.3.4.3 internal `$sscanf` /
  // `$fscanf` EOF guard. Runtime exposes `HasUnknown()` uniformly on
  // every packed value type (always-false impl on 2-state); the call
  // shape is uniform and downstream constant-folds the 2-state case.
  kIsUnknown,
  // Observable storage cell (`Var<T>` / `ExternalRef<T>`). The cell is
  // the receiver; `Set` / `Mutate` take services as the second argument.
  kGet,
  kSet,
  kMutate,
  // Scope handle -- reaches the engine facade `RuntimeServices` so every
  // runtime-effect call can thread it as a plain argument
  // (`decisions/runtime-effects-as-generic-calls.md`).
  kServices,
};

// Instance method or free-function call against a closed-namespace
// runtime entry. The receiver, if any, is `args[0]`; remaining args are
// the method parameters. The (`BuiltinFn`, `args[0].type`) pair fully
// identifies the runtime symbol; the per-backend realization table maps
// it to a method, free-function, or other C++ shape.
struct BuiltinFnCallee {
  BuiltinFn id;
};

// Type-static runtime entry -- no value receiver. `type_qual` names the
// type whose static is invoked (e.g. `MyEnum::First()`), and is part of
// the symbol's identity that LLVM also reads for mangling.
struct BuiltinStaticCallee {
  BuiltinFn id;
  TypeId type_qual;
};

// Whether a `BuiltinFn` mutates its receiver in place. Drives the
// LHS-chain walker that decides whether an observable receiver needs the
// `Mutate` wrap. Same predicate role as `IsMutatingBuiltinMethod` for the
// legacy callee shape.
[[nodiscard]] auto IsMutatingBuiltinFn(BuiltinFn id) -> bool;

// Whether a `BuiltinFn` is an indexed / sliced container access whose
// receiver is `args[0]`. LHS-chain walkers use this to reach the root
// primary.
[[nodiscard]] auto IsContainerAccessFn(BuiltinFn id) -> bool;

}  // namespace lyra::mir
