#pragma once

#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/unary_op.hpp"

namespace lyra::hir {

struct PrimaryExpr {
  Primary data;
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

enum class AssignKind : std::uint8_t {
  kBlocking,
  kNonBlocking,
};

struct AssignExpr {
  AssignKind kind;
  Lvalue lhs;
  ExprId rhs;
};

struct CallExpr {
  SubroutineRef callee;
  std::vector<ExprId> arguments;
};

struct InsideRangePair {
  ExprId lo;
  ExprId hi;
};

using InsideItem = std::variant<ExprId, InsideRangePair>;

struct InsideExpr {
  ExprId lhs;
  std::vector<InsideItem> items;
};

struct ElementSelectExpr {
  ExprId base_value;
  ExprId index;
};

struct RangeSelectExpr {
  ExprId base_value;
  RangeBounds bounds;
};

using ExprData = std::variant<
    PrimaryExpr, UnaryExpr, BinaryExpr, ConditionalExpr, AssignExpr, CallExpr,
    ConversionExpr, InsideExpr, ElementSelectExpr, RangeSelectExpr>;

struct Expr {
  TypeId type;
  ExprData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
