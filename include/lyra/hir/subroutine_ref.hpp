#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

struct StructuralSubroutineRef {
  StructuralHops hops;
  StructuralSubroutineId subroutine;
};

struct SystemSubroutineRef {
  support::SystemSubroutineId id;
};

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

using SubroutineRef = std::variant<
    StructuralSubroutineRef, SystemSubroutineRef, BuiltinMethodRef>;

}  // namespace lyra::hir
