#include "lyra/lowering/ast_to_hir/expression/assignment.hpp"

#include <concepts>
#include <expected>
#include <optional>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/support/builtin_fn.hpp"

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

// Whether `value` names the object the running method was invoked on (LRM
// 8.11).
auto NamesOwnObject(const slang::ast::Expression& value) -> bool {
  const auto* named = value.as_if<slang::ast::NamedValueExpression>();
  if (named == nullptr) return false;
  const slang::ast::Scope* scope = named->symbol.getParentScope();
  const auto* method =
      scope == nullptr
          ? nullptr
          : scope->asSymbol().as_if<slang::ast::SubroutineSymbol>();
  return method != nullptr && method->thisVar == &named->symbol;
}

// The property an assignment writes where it is one Lyra's definitions hold
// weakly (LRM 8.30): one of the method's own object, named alone or through
// `this`.
auto WeaklyHeldTarget(const slang::ast::Expression& target)
    -> const slang::ast::ClassPropertySymbol* {
  const slang::ast::Symbol* member = nullptr;
  if (const auto* named = target.as_if<slang::ast::NamedValueExpression>()) {
    member = &named->symbol;
  } else if (const auto* access =
                 target.as_if<slang::ast::MemberAccessExpression>();
             access != nullptr && NamesOwnObject(access->value())) {
    member = &access->member;
  }
  const auto* prop = member == nullptr
                         ? nullptr
                         : member->as_if<slang::ast::ClassPropertySymbol>();
  return prop != nullptr && IsHeldWeakly(*prop) ? prop : nullptr;
}

// A write of a handle into a weakly held property: the weak reference to the
// object the handle names, made by the library, is what lands (LRM 8.30.2,
// 8.30.4).
template <ExprLowerer Lowerer>
auto LowerStoreWeaklyHeld(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentExpression& as,
    const slang::ast::ClassPropertySymbol& prop, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (as.op.has_value() || as.isNonBlocking() || as.timingControl != nullptr) {
    throw InternalError(
        "LowerAssignmentExpr: Lyra's definitions write a weakly held property "
        "only by a plain blocking assignment");
  }
  auto& unit_lowerer = lowerer.Owner();
  auto stored = LowerWeaklyHeldStorage(unit_lowerer, prop, span);
  if (!stored) return std::unexpected(std::move(stored.error()));
  const hir::TypeId held = stored->type;
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(stored));
  auto rhs_or = lowerer.LowerExpr(as.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId strong = frame.Exprs().Add(*std::move(rhs_or));
  const hir::ExprId weak = frame.Exprs().Add(
      hir::Expr{
          .type = held,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = support::BuiltinFn::kWeakDowngrade,
                          .receiver = strong},
                  .arguments = {}},
          .span = span});
  return hir::Expr{
      .type = held,
      .data =
          hir::AssignExpr{
              .timing = hir::EffectTiming{hir::ImmediateEffect{}},
              .lhs = lhs_id,
              .compound = std::nullopt,
              .rhs = weak},
      .span = span,
  };
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerAssignmentExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();

  if (const auto* weak = WeaklyHeldTarget(as.left())) {
    return LowerStoreWeaklyHeld(lowerer, frame, as, *weak, span);
  }

  auto lhs_or = lowerer.LowerExpr(as.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));

  auto type_id = unit_lowerer.InternExpressionType(as, span);
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
                .compound = std::nullopt,
                .rhs = rhs_id},
        .span = span,
    };
  }

  if (as.isNonBlocking()) {
    throw InternalError(
        "LowerAssignmentExpr: compound assignment with non-blocking "
        "operator is not a legal SV form (LRM A.6.2 grammar)");
  }

  const CompoundExpansion applied = CompoundExpansionOf(as.right());
  auto rhs_or = lowerer.LowerExpr(*applied.operand, frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
  auto applied_at = unit_lowerer.InternResultType(*applied.applied_at, span);
  if (!applied_at) return std::unexpected(std::move(applied_at.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignExpr{
              .timing = timing,
              .lhs = lhs_id,
              .compound =
                  hir::CompoundAssignOperator{
                      .op = LowerBinaryOp(*as.op), .applied_at = *applied_at},
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

  auto type_id = unit_lowerer.InternExpressionType(un, span);
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
