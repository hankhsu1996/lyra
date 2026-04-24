#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/hir/primary.hpp"

namespace lyra::hir {

struct ExprId {
  std::uint32_t value;

  auto operator<=>(const ExprId&) const -> std::strong_ordering = default;
};

using ExprData = std::variant<Primary>;

struct Expr {
  ExprData data;
};

}  // namespace lyra::hir
