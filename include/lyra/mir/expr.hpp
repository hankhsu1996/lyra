#pragma once

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builtin_method_kind.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/lvalue.hpp"
#include "lyra/mir/range_bounds.hpp"
#include "lyra/mir/runtime_diagnostic.hpp"
#include "lyra/mir/runtime_finish.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_subroutine.hpp"
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

struct RealLiteral {
  double value;
};

struct UnaryExpr {
  UnaryOp op;
  ExprId operand;
};

struct BinaryExpr {
  BinaryOp op;
  ExprId lhs;
  ExprId rhs;
};

struct ConditionalExpr {
  ExprId condition;
  ExprId then_value;
  ExprId else_value;
};

struct AssignExpr {
  Lvalue target;
  ExprId value;
};

struct SystemSubroutineCallee {
  support::SystemSubroutineId id;
};

struct BuiltinMethodCallee {
  TypeId receiver_type;
  BuiltinMethodKind kind;
};

using Callee = std::variant<
    SystemSubroutineCallee, StructuralSubroutineRef, BuiltinMethodCallee>;

struct CallExpr {
  Callee callee;
  std::vector<ExprId> arguments;
};

using RuntimeCall = std::variant<
    RuntimePrintCall, RuntimeDiagnosticCall, RuntimeFinishCall,
    RuntimeSubmitObservedCall, RuntimeSubmitNbaCall>;

struct RuntimeCallExpr {
  RuntimeCall call;
};

struct ElementSelectExpr {
  ExprId base_value;
  ExprId index;
};

struct RangeSelectExpr {
  ExprId base_value;
  RangeBounds bounds;
};

struct ConcatExpr {
  std::vector<ExprId> operands;
};

// LRM 11.4.12 / 11.4.12.2: `{multiplier{...}}`. `concat` always points to a
// ConcatExpr; mirrors the hir::ReplicationExpr shape.
struct ReplicationExpr {
  ExprId count;
  ExprId concat;
};

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, RealLiteral, StructuralParamRef,
    StructuralVarRef, ProceduralVarRef, UnaryExpr, BinaryExpr, ConditionalExpr,
    AssignExpr, CallExpr, RuntimeCallExpr, ConversionExpr, ClosureExpr,
    ElementSelectExpr, RangeSelectExpr, ConcatExpr, ReplicationExpr>;

struct Expr {
  ExprData data;
  TypeId type;
};

}  // namespace lyra::mir
