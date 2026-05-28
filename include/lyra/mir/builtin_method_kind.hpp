#pragma once

#include <cstdint>

namespace lyra::mir {

enum class BuiltinMethodKind : std::uint8_t {
  kEnumFirst,
  kEnumLast,
  kEnumNum,
  kEnumNext,
  kEnumPrev,
  kEnumName,
};

}  // namespace lyra::mir
