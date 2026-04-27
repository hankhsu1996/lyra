#pragma once

#include <compare>
#include <cstdint>
#include <string>

namespace lyra::hir {

struct LoopVarDeclId {
  std::uint32_t value;

  auto operator<=>(const LoopVarDeclId&) const
      -> std::strong_ordering = default;
};

struct LoopVarDecl {
  std::string name;
};

}  // namespace lyra::hir
