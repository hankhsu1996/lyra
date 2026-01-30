#include "lyra/lowering/ast_to_hir/pattern.hpp"

#include <slang/ast/expressions/AssignmentExpressions.h>

#include "lyra/hir/arena.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerPattern(
    const slang::ast::StructuredAssignmentPatternExpression& pattern,
    TypeId /*target_type_id*/, SourceSpan span, ExpressionLoweringView view)
    -> hir::PatternId {
  auto* ctx = view.context;

  // Milestone 1: only handle default-only patterns
  if (!pattern.indexSetters.empty() || !pattern.typeSetters.empty()) {
    ctx->sink->Error(
        span, "assignment pattern with index/type setters not yet supported");
    return hir::kInvalidPatternId;
  }

  if (pattern.defaultSetter == nullptr) {
    ctx->sink->Error(span, "assignment pattern requires default setter");
    return hir::kInvalidPatternId;
  }

  // Check if default is unbased-unsized literal (bit-level fill)
  bool is_bit_fill = pattern.defaultSetter->kind ==
                     slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral;

  // Lower the default expression
  hir::ExpressionId fill_expr = LowerExpression(*pattern.defaultSetter, view);
  if (!fill_expr) {
    return hir::kInvalidPatternId;
  }

  // Create Fill pattern (target_type_id already lowered by caller)
  return ctx->hir_arena->AddPattern(
      hir::Pattern{
          .kind = hir::PatternKind::kFill,
          .span = span,
          .data = hir::FillPatternData{
              .fill_expr = fill_expr, .is_bit_fill = is_bit_fill}});
}

}  // namespace lyra::lowering::ast_to_hir
