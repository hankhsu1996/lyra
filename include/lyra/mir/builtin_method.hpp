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

}  // namespace lyra::mir
