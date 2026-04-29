#pragma once

#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct PrimaryExpr {
  Primary data;
};

struct BinaryExpr {
  BinaryOp op;
  ExprId lhs;
  ExprId rhs;
};

struct AssignExpr {
  ExprId lhs;
  ExprId rhs;
};

struct CallExpr {
  SubroutineRef callee;
  std::vector<ExprId> arguments;
};

using ExprData =
    std::variant<PrimaryExpr, BinaryExpr, AssignExpr, CallExpr, ConversionExpr>;

struct Expr {
  TypeId type;
  ExprData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
