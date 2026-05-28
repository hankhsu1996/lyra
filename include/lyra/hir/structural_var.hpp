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

// SV LRM 10.4 + 6.21: structural variable declarations may carry an
// initializer expression evaluated at construction time (before any process
// runs). The initializer lives in the enclosing scope's `exprs` table; we
// reference it by id so the decl stays a flat row alongside other vars.
struct StructuralVarDecl {
  std::string name;
  TypeId type;
  std::optional<ExprId> initializer;
};

}  // namespace lyra::hir
