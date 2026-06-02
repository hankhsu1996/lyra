#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

struct ProceduralVarId {
  std::uint32_t value;

  auto operator<=>(const ProceduralVarId&) const
      -> std::strong_ordering = default;
};

// LRM 13.3.1 / 13.4.2 variable lifetime. A static-lifetime variable has one
// storage location per module instance that retains its value between calls;
// an automatic-lifetime variable is allocated fresh for each activation. slang
// resolves the source keyword and the enclosing module / subroutine default
// into a per-variable choice, which HIR records verbatim.
enum class VariableLifetime : std::uint8_t {
  kStatic,
  kAutomatic,
};

struct ProceduralVarDecl {
  std::string name;
  TypeId type;
  VariableLifetime lifetime = VariableLifetime::kAutomatic;
};

}  // namespace lyra::hir
