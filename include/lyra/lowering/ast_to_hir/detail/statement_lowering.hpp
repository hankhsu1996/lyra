#pragma once

#include <optional>

#include <slang/text/SourceLocation.h>

#include "lyra/common/source_span.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace slang::ast {
class Statement;
}

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

// Normalized result of lowering and validating a deferred assertion action
// branch (pass or fail). Produced once after child-action lowering; consumed
// by deferred assertion semantic construction. Moves syntax-shape ownership
// out of feature-specific deferred code.
enum class LoweredActionBranchKind {
  kAbsent,           // no branch present in source
  kValidSingleCall,  // branch is a supported single user subroutine call
  kInvalid,  // branch present but unsupported shape (diagnostic emitted)
};

struct LoweredActionBranch {
  LoweredActionBranchKind kind = LoweredActionBranchKind::kAbsent;
  std::optional<hir::StatementId> statement_id;
  const hir::CallExpressionData* hir_call = nullptr;
};

// Lower an optional child action statement and normalize it into a
// LoweredActionBranch. Validates the supported single-call grammar and
// extracts the HIR CallExpressionData.
auto LowerAndNormalizeActionBranch(
    const slang::ast::Statement* slang_action, ScopeLowerer& lowerer,
    const hir::Arena& arena, SourceSpan span, const char* branch_label)
    -> LoweredActionBranch;

}  // namespace lyra::lowering::ast_to_hir
