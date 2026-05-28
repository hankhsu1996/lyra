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
};

struct BuiltinMethodRef {
  BuiltinMethodKind kind;
};

}  // namespace lyra::hir
