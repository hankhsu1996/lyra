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

struct BuiltinMethodRef {
  std::variant<EnumMethodKind, StringMethodKind, EventMethodKind> method;
};

}  // namespace lyra::hir
