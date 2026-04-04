#pragma once

#include <slang/text/SourceLocation.h>

#include "lyra/common/source_span.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace lyra::lowering::ast_to_hir {

// Common context bundle for statement lowering helpers.
// Eliminates the repeated registrar/ctx/frame/span/lower_expr boilerplate.
// Modeled after ExpressionLoweringView.
struct StatementLoweringEnv {
  ScopeLowerer* lowerer;
  SymbolRegistrar* registrar;
  Context* ctx;
  const LoweringFrame* frame;
  SourceSpan span;

  explicit StatementLoweringEnv(
      ScopeLowerer& lowerer_ref, slang::SourceRange source_range)
      : lowerer(&lowerer_ref),
        registrar(&lowerer_ref.Registrar()),
        ctx(&lowerer_ref.Ctx()),
        frame(&lowerer_ref.Frame()),
        span(lowerer_ref.Ctx().SpanOf(source_range)) {
  }

  [[nodiscard]] auto LowerExpr(const slang::ast::Expression& expr) const
      -> hir::ExpressionId {
    return LowerScopedExpression(expr, *ctx, *registrar, *frame);
  }
};

}  // namespace lyra::lowering::ast_to_hir
