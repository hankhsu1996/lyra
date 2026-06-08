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

// LRM 7.5.2 / 7.5.3 dynamic-array methods plus the LRM 7.12.2 / 7.12.3
// no-`with`, no-queue-return subset of the array-method family. The arms
// are named for "array" rather than "dynamic array" because the method
// semantics are LRM-uniform across container kinds (fixed unpacked, queue);
// only the AST -> HIR receiver-detection block differs per container, and
// other containers will extend the routing without renaming this enum.
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
};

// LRM 7.12.4 iterator intrinsic methods (only `index` is in scope today;
// extends naturally if SV adds more iterator methods).
enum class IteratorMethodKind : std::uint8_t {
  kIndex,
};

struct BuiltinMethodRef {
  std::variant<
      EnumMethodKind, StringMethodKind, EventMethodKind, ArrayMethodKind,
      IteratorMethodKind>
      method;
};

}  // namespace lyra::hir
