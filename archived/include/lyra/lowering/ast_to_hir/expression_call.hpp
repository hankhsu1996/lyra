#pragma once

#include <slang/ast/Expression.h>

#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerCallExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
