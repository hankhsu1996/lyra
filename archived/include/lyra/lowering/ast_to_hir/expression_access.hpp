#pragma once

#include <slang/ast/Expression.h>

#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerConversionExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId;

auto LowerElementSelectExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId;

auto LowerRangeSelectExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId;

auto LowerMemberAccessExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
