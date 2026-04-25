#pragma once

#include <functional>
#include <optional>
#include <variant>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"

namespace lyra::hir {

[[nodiscard]] inline auto AsAssignableRef(const Expr& expr)
    -> std::optional<std::reference_wrapper<const RefExpr>> {
  const auto* primary = std::get_if<PrimaryExpr>(&expr.data);
  if (primary == nullptr) return std::nullopt;
  const auto* ref = std::get_if<RefExpr>(&primary->data);
  if (ref == nullptr) return std::nullopt;
  return std::cref(*ref);
}

}  // namespace lyra::hir
