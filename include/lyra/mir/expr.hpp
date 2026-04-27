#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>
#include <vector>

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

struct StringLiteral {
  std::string value;
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
};

struct AssignExpr {
  Lvalue target;
  ExprId value;
};

struct UserSubroutineTargetId {
  std::uint32_t value;

  auto operator<=>(const UserSubroutineTargetId&) const
      -> std::strong_ordering = default;
};

struct UserSubroutineTarget {
  std::string name;
};

using Callee = UserSubroutineTargetId;

struct CallExpr {
  Callee callee;
  std::vector<ExprId> arguments;
};

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, MemberVarRef, LocalVarRef, BinaryExpr,
    AssignExpr, CallExpr>;

struct Expr {
  ExprData data;
  TypeId type;
};

}  // namespace lyra::mir
