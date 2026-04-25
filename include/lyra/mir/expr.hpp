#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct ExprId {
  std::uint32_t value;

  auto operator<=>(const ExprId&) const -> std::strong_ordering = default;
};

struct IntegerLiteral {
  std::int64_t value;
};

struct MemberVarRef {
  MemberVarId target;
};

struct LocalVarRef {
  LocalVarId target;
};

using Lvalue = std::variant<MemberVarRef, LocalVarRef>;

struct BinaryExpr {
  BinaryOp op;
  ExprId lhs;
  ExprId rhs;
  TypeId type;
};

struct AssignExpr {
  Lvalue target;
  ExprId value;
  TypeId type;
};

using ExprData = std::variant<
    IntegerLiteral, MemberVarRef, LocalVarRef, BinaryExpr, AssignExpr>;

struct Expr {
  ExprData data;
};

}  // namespace lyra::mir
