#pragma once

#include <cstdint>

namespace lyra::hir {

enum class BuiltinMethodKind : std::uint8_t {
  kEnumFirst,
  kEnumLast,
  kEnumNum,
  kEnumNext,
  kEnumPrev,
  kEnumName,
  // LRM 15.5 named event operations. Trigger / Await arise from collapsing
  // `-> e;` and `@e;` at HIR -> MIR; Triggered surfaces from slang's
  // `e.triggered` method call.
  kNamedEventTrigger,
  kNamedEventAwait,
  kNamedEventTriggered,
};

struct BuiltinMethodRef {
  BuiltinMethodKind kind;
};

}  // namespace lyra::hir
