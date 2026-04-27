#pragma once

#include <compare>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct ExprId {
  std::uint32_t value;

  auto operator<=>(const ExprId&) const -> std::strong_ordering = default;
};

struct PrimaryExpr {
  Primary data;
};

struct BinaryExpr {
  BinaryOp op;
  ExprId lhs;
  ExprId rhs;
  TypeId type;
};

struct AssignExpr {
  ExprId lhs;
  ExprId rhs;
  TypeId type;
};

struct CallExpr {
  SubroutineRef callee;
  std::vector<ExprId> arguments;
  TypeId result_type;
};

using ExprData = std::variant<PrimaryExpr, BinaryExpr, AssignExpr, CallExpr>;

struct Expr {
  ExprData data;
};

}  // namespace lyra::hir
