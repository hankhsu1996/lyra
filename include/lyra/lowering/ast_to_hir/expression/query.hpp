#pragma once

// Lowering of the LRM 20.6 data-query and 20.7 array-query system functions
// (`$bits`, `$typename`, `$left` / `$right` / `$low` / `$high` / `$increment` /
// `$size`, `$dimensions` / `$unpacked_dimensions`).

#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class CallExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A query reports a property of a type or of an array's shape rather than
// computing over a value, so its operand is never evaluated (LRM 20.6.1,
// 20.6.2) and may be a data type, which has no value form. The call is
// therefore resolved ahead of the argument loop, and returns nullopt when it is
// not a query.
//
// Whatever the operand's type fixes, the query reports as an elaboration-time
// constant: a type's bit count or name, a dimension count, and every property
// of a fixed-size dimension -- and also a dynamically sized dimension's
// direction, which its kind fixes even though its extent is unknown. What is
// left is the extent itself, which the query reads from the value's current
// state as an ordinary expression over the container's element count.
//
// A dimension the running simulation names (LRM 20.7 does not require a
// constant one) selects among those per-dimension constants, which the
// operand's type still fixes whichever of them the index lands on.
template <ExprLowerer Lowerer>
auto LowerQueryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<std::optional<hir::Expr>>;

}  // namespace lyra::lowering::ast_to_hir
