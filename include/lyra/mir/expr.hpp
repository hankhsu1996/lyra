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
#include "lyra/support/system_subroutine.hpp"

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

struct PrintBuiltinInfo {
  support::PrintRadix radix;
  bool append_newline;
  bool is_strobe;
  support::PrintSinkKind sink_kind;
};

using BuiltinOp = std::variant<PrintBuiltinInfo>;

struct UserSubroutineTargetId {
  std::uint32_t value;

  auto operator<=>(const UserSubroutineTargetId&) const
      -> std::strong_ordering = default;
};

struct UserSubroutineTarget {
  std::string name;
};

using Callee = std::variant<UserSubroutineTargetId, BuiltinOp>;

struct CallExpr {
  Callee callee;
  std::vector<ExprId> arguments;
  TypeId result_type;
};

using ExprData = std::variant<
    IntegerLiteral, MemberVarRef, LocalVarRef, BinaryExpr, AssignExpr,
    CallExpr>;

struct Expr {
  ExprData data;
};

}  // namespace lyra::mir
