#pragma once

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_subroutine.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/system_subroutine.hpp"

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

struct StructuralVarRef {
  StructuralHops hops;
  StructuralVarId var;
};

struct ProceduralVarRef {
  ProceduralHops hops;
  ProceduralVarId var;
};

using Lvalue = std::variant<StructuralVarRef, ProceduralVarRef>;

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

struct SystemSubroutineCallee {
  support::SystemSubroutineId id;
};

using Callee = std::variant<SystemSubroutineCallee, StructuralSubroutineRef>;

struct CallExpr {
  Callee callee;
  std::vector<ExprId> arguments;
};

struct RuntimeCallExpr {
  RuntimePrintCall print;
};

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, StructuralParamRef,
    StructuralVarRef, ProceduralVarRef, UnaryExpr, BinaryExpr, AssignExpr,
    CallExpr, RuntimeCallExpr, ConversionExpr>;

struct Expr {
  ExprData data;
  TypeId type;
};

}  // namespace lyra::mir
