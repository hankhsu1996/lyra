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

// True for methods whose backend emission must wrap the call site in
// `co_await`. The runtime returns an awaitable that submits the appropriate
// WaitRequest via the coroutine's promise.
[[nodiscard]] inline auto IsSuspending(const EventMethodInfo& info) -> bool {
  return info.kind == EventMethodKind::kAwait;
}

struct BuiltinMethodCallee {
  std::variant<EnumMethodInfo, StringMethodInfo, EventMethodInfo> method;
};

}  // namespace lyra::mir
