#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct LoopVarDeclId {
  std::uint32_t value;

  auto operator<=>(const LoopVarDeclId&) const
      -> std::strong_ordering = default;
};

struct LoopVarDecl {
  std::string name;
  TypeId type;
};

}  // namespace lyra::hir
