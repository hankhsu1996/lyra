#pragma once

#include <cstdint>
#include <variant>

namespace lyra::hir {

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

// LRM 15.5 named event operations. Trigger / Await arise from collapsing
// `-> e;` and `@e;` at HIR -> MIR; Triggered surfaces from slang's
// `e.triggered` method call.
enum class EventMethodKind : std::uint8_t {
  kTrigger,
  kAwait,
  kTriggered,
};

// LRM 7.5.2 / 7.5.3 plus the LRM 7.12 array-method family, uniform across
// container kinds -- hence "array", not "dynamic array".
enum class ArrayMethodKind : std::uint8_t {
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

// LRM 7.9 associative-array query and traversal methods.
enum class AssociativeMethodKind : std::uint8_t {
  kNum,
  kSize,
  kExists,
  kDelete,
  kFirst,
  kLast,
  kNext,
  kPrev,
};

// LRM 7.12.4 iterator intrinsic methods.
enum class IteratorMethodKind : std::uint8_t {
  kIndex,
};

struct BuiltinMethodRef {
  std::variant<
      EnumMethodKind, StringMethodKind, EventMethodKind, ArrayMethodKind,
      QueueMethodKind, AssociativeMethodKind, IteratorMethodKind>
      method;
};

}  // namespace lyra::hir
