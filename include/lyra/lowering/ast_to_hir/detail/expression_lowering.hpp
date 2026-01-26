#pragma once

#include <slang/ast/Expression.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
struct LoweringFrame;
class SymbolRegistrar;

/// Internal view for expression lowering helpers.
/// Frame is nullptr for design-level context, non-null for scoped context.
///
/// Implementation detail - use LowerDesignLevelExpression/LowerScopedExpression
/// from expression.hpp at module boundaries.
struct ExpressionLoweringView {
  Context* context;
  SymbolRegistrar* registrar;
  const LoweringFrame* frame;  // nullptr = design-level, non-null = scoped
};

/// Internal dispatcher for recursive expression lowering.
/// Implementation detail - use
/// LowerDesignLevelExpression/LowerScopedExpression.
auto LowerExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
