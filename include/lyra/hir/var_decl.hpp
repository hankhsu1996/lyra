#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct VarDeclId {
  std::uint32_t value;

  auto operator<=>(const VarDeclId&) const -> std::strong_ordering = default;
};

struct VarDecl {
  std::string name;
  TypeId type;
};

}  // namespace lyra::hir
