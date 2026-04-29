#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::mir {

enum class TimeScale : std::uint8_t { kFs, kPs, kNs, kUs, kMs, kS };

struct IntegerLiteral {
  IntegralConstant value;
};

struct StringLiteral {
  std::string value;
};

struct TimeLiteral {
  double value;
  TimeScale scale;
};

struct MemberVarRef {
  MemberVarId target;
};

struct LocalVarRef {
  LocalScopeId scope;
  LocalVarId local;
};

using Lvalue = std::variant<MemberVarRef, LocalVarRef>;

struct UnaryExpr {
  UnaryOp op;
  ExprId operand;
};

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

struct RuntimeCallExpr {
  RuntimePrintCall print;
};

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, MemberVarRef, LocalVarRef,
    UnaryExpr, BinaryExpr, AssignExpr, CallExpr, RuntimeCallExpr,
    ConversionExpr>;

struct Expr {
  ExprData data;
  TypeId type;
};

}  // namespace lyra::mir
