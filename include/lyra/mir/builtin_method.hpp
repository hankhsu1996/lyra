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

// LRM 7.5.2 / 7.5.3 dynamic-array methods plus the LRM 7.12.2 / 7.12.3
// no-`with`, no-queue-return subset of the array-method family.
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

// Side-effect-free queries that are defined for every runtime value type
// (string, packed integral, unpacked array, ...). The receiver's MIR type
// alone determines the emit shape; lowering passes the operand uniformly.
// The LRM 21.3.4.3 "unknown bits in $sscanf str / format imply EOF" guard
// lowers to kIsUnknown; future LRM 20.9 `$isunknown` lowers to the same
// leaf.
enum class ValueMethodKind : std::uint8_t {
  kIsUnknown,
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

struct ValueMethodInfo {
  ValueMethodKind kind;
};

// True for methods whose backend emission must wrap the call site in
// `co_await`. The runtime returns an awaitable that suspends the calling
// coroutine and reschedules it through RuntimeServices when the event fires.
[[nodiscard]] inline auto IsSuspending(const EventMethodInfo& info) -> bool {
  return info.kind == EventMethodKind::kAwait;
}

struct BuiltinMethodCallee {
  std::variant<
      EnumMethodInfo, StringMethodInfo, EventMethodInfo, ArrayMethodInfo,
      ValueMethodInfo>
      method;
};

}  // namespace lyra::mir
