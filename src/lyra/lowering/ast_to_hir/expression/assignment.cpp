#include "lyra/lowering/ast_to_hir/expression/assignment.hpp"

#include <concepts>
#include <expected>
#include <optional>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/lowering/ast_to_hir/event_handle.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// LRM 9.4.5: when the update happens. A nonblocking assignment's control says
// which slot's NBA region the update lands in and leaves the procedure running;
// a blocking one suspends the procedure, which is a statement rather than an
// expression, so it is expanded into the equivalent statement sequence before
// any expression is built.
template <ExprLowerer Lowerer>
auto LowerAssignTiming(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::EffectTiming> {
  if (as.timingControl == nullptr) {
    return as.isNonBlocking() ? hir::EffectTiming{hir::NonBlockingEffect{}}
                              : hir::EffectTiming{hir::ImmediateEffect{}};
  }
  // LRM 11.4.1 admits an intra-assignment control only where a procedural
  // statement stands, so a write that a construction performs cannot carry
  // one and there is no time for it to name.
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    if (!as.isNonBlocking()) {
      throw InternalError(
          "LowerAssignTiming: a blocking assignment carrying an "
          "intra-assignment timing control reached expression lowering "
          "unexpanded");
    }
    auto control =
        LowerDelayOrEventControl(lowerer, frame, *as.timingControl, span);
    if (!control) return std::unexpected(std::move(control.error()));
    return hir::EffectTiming{
        hir::NonBlockingEffect{.control = *std::move(control)}};
  } else {
    throw InternalError(
        "LowerAssignTiming: an assignment outside procedural code carries an "
        "intra-assignment timing control");
  }
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerAssignmentExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();

  if (auto refused = RefuseGivingAnEventAValue(*as.left().type, span);
      !refused) {
    return std::unexpected(std::move(refused.error()));
  }

  auto lhs_or = lowerer.LowerExpr(as.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));

  auto type_id = unit_lowerer.InternType(*as.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  auto timing_or = LowerAssignTiming(lowerer, frame, as, span);
  if (!timing_or) return std::unexpected(std::move(timing_or.error()));
  const hir::EffectTiming timing = *std::move(timing_or);

  if (!as.op.has_value()) {
    auto rhs_or = lowerer.LowerExpr(as.right(), frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::AssignExpr{
                .timing = timing,
                .lhs = lhs_id,
                .compound_op = std::nullopt,
                .rhs = rhs_id},
        .span = span,
    };
  }

  if (as.isNonBlocking()) {
    throw InternalError(
        "LowerAssignmentExpr: compound assignment with non-blocking "
        "operator is not a legal SV form (LRM A.6.2 grammar)");
  }

  const auto& bare_user_rhs = BareCompoundUserRhs(as.right());
  auto rhs_or = lowerer.LowerExpr(bare_user_rhs, frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  hir::Expr rhs_expr = *std::move(rhs_or);
  if (rhs_expr.type.value != type_id->value) {
    const hir::ExprId inner_id = frame.Exprs().Add(std::move(rhs_expr));
    rhs_expr = hir::Expr{
        .type = *type_id,
        .data =
            hir::ConversionExpr{
                .kind = hir::ConversionKind::kImplicit, .operand = inner_id},
        .span = span,
    };
  }
  const hir::ExprId rhs_id = frame.Exprs().Add(std::move(rhs_expr));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignExpr{
              .timing = timing,
              .lhs = lhs_id,
              .compound_op = LowerBinaryOp(*as.op),
              .rhs = rhs_id},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerIncDecExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::UnaryExpression& un,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();

  auto target_or = lowerer.LowerExpr(un.operand(), frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const hir::ExprId target_id = frame.Exprs().Add(*std::move(target_or));

  auto type_id = unit_lowerer.InternType(*un.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  return hir::Expr{
      .type = *type_id,
      .data =
          hir::IncDecExpr{.op = LowerSlangIncDecOp(un.op), .target = target_id},
      .span = span,
  };
}

template auto LowerAssignmentExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::AssignmentExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerAssignmentExpr(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::AssignmentExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerIncDecExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::UnaryExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerIncDecExpr(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::UnaryExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
