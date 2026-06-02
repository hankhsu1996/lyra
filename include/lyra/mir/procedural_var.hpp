#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct ProceduralVarId {
  std::uint32_t value;

  auto operator<=>(const ProceduralVarId&) const
      -> std::strong_ordering = default;
};

// LRM 13.3.1 / 13.4.2 variable lifetime. A kStatic var has one storage slot
// per module instance that the body shares across activations; a kAutomatic var
// lives in the activation (a C++ stack local). The lifetime is a semantic
// property; the backend chooses the placement it implies.
enum class VariableLifetime : std::uint8_t {
  kStatic,
  kAutomatic,
};

struct ProceduralVarDecl {
  std::string name;
  TypeId type;
  VariableLifetime lifetime = VariableLifetime::kAutomatic;
};

}  // namespace lyra::mir
