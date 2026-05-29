#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

struct StructuralVarId {
  std::uint32_t value;

  auto operator<=>(const StructuralVarId&) const
      -> std::strong_ordering = default;
};

struct StructuralVarDecl {
  std::string name;
  TypeId type;
  std::optional<ExprId> initializer;
};

}  // namespace lyra::hir
