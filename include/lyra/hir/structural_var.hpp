#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct StructuralVarId {
  std::uint32_t value;

  auto operator<=>(const StructuralVarId&) const
      -> std::strong_ordering = default;
};

struct StructuralVarDecl {
  std::string name;
  TypeId type;
};

}  // namespace lyra::hir
