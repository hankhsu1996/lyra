#include "lyra/lowering/ast_to_hir/pattern.hpp"

#include <slang/ast/expressions/AssignmentExpressions.h>

#include "lyra/common/type_queries.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerPattern(
    const slang::ast::StructuredAssignmentPatternExpression& pattern,
    TypeId target_type_id, SourceSpan span, ExpressionLoweringView view)
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

  // Only packed types supported for now
  if (!IsPackedIntegralLike(target_type_id, *ctx->type_arena)) {
    ctx->sink->Error(
        span, "assignment pattern for non-packed types not yet supported");
    return hir::kInvalidPatternId;
  }

  // Compute fill shape to get unit_type
  // For kIntegral targets, we need a 1-bit type matching the 4-state nature
  const Type& target_type = (*ctx->type_arena)[target_type_id];
  bool is_four_state = IsPackedFourState(target_type, *ctx->type_arena);
  TypeId bit_type = ctx->type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{
          .bit_width = 1, .is_signed = false, .is_four_state = is_four_state});
  PackedFillShape shape =
      ComputePackedFillShape(target_type_id, *ctx->type_arena, bit_type);

  // Lower and coerce setter to unit_type (handles tick literals globally)
  const slang::ast::Expression& setter = *pattern.defaultSetter;
  hir::ExpressionId fill_expr = LowerAndCoerce(setter, shape.unit_type, view);
  if (!fill_expr) {
    return hir::kInvalidPatternId;
  }

  return ctx->hir_arena->AddPattern(
      hir::Pattern{
          .kind = hir::PatternKind::kFill,
          .span = span,
          .data = hir::FillPatternData{.fill_expr = fill_expr}});
}

}  // namespace lyra::lowering::ast_to_hir
