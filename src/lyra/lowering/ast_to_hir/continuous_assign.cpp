#include "lyra/hir/continuous_assign.hpp"

#include <expected>
#include <optional>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/strength.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The delays a continuous assignment is written with (LRM 10.3.3, 28.16), as
// many as the source gave.
auto LowerAssignmentDelay(
    StructuralScopeLowerer& scope, const slang::ast::TimingControl& delay,
    WalkFrame frame) -> diag::Result<hir::AssignmentDelay> {
  const auto amount =
      [&](const slang::ast::Expression& expr) -> diag::Result<hir::ExprId> {
    auto lowered = scope.LowerExpr(expr, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return frame.Exprs().Add(*std::move(lowered));
  };
  switch (delay.kind) {
    case slang::ast::TimingControlKind::Delay: {
      auto single = amount(delay.as<slang::ast::DelayControl>().expr);
      if (!single) return std::unexpected(std::move(single.error()));
      return hir::SingleDelay{.amount = *single};
    }
    case slang::ast::TimingControlKind::Delay3: {
      const auto& written = delay.as<slang::ast::Delay3Control>();
      auto rise = amount(written.expr1);
      if (!rise) return std::unexpected(std::move(rise.error()));
      if (written.expr2 == nullptr) {
        return hir::SingleDelay{.amount = *rise};
      }
      auto fall = amount(*written.expr2);
      if (!fall) return std::unexpected(std::move(fall.error()));
      if (written.expr3 == nullptr) {
        return hir::RiseFallDelay{.rise = *rise, .fall = *fall};
      }
      auto turn_off = amount(*written.expr3);
      if (!turn_off) return std::unexpected(std::move(turn_off.error()));
      return hir::RiseFallTurnOffDelay{
          .rise = *rise, .fall = *fall, .turn_off = *turn_off};
    }
    // LRM A.2.2.3: a continuous assignment and a net declaration take a
    // `delay3` and nothing else, so the front end binds no other control here.
    case slang::ast::TimingControlKind::Invalid:
    case slang::ast::TimingControlKind::SignalEvent:
    case slang::ast::TimingControlKind::EventList:
    case slang::ast::TimingControlKind::ImplicitEvent:
    case slang::ast::TimingControlKind::RepeatedEvent:
    case slang::ast::TimingControlKind::OneStepDelay:
    case slang::ast::TimingControlKind::CycleDelay:
    case slang::ast::TimingControlKind::BlockEventList:
      break;
  }
  throw InternalError(
      "LowerAssignmentDelay: the grammar gives a continuous assignment and a "
      "net declaration only a delay or a list of up to three delays");
}

}  // namespace

auto StructuralScopeLowerer::BuildContinuousAssign(
    diag::SourceSpan span, hir::Expr lhs, hir::Expr rhs,
    const slang::ast::Expression& driving, const slang::ast::Symbol& containing,
    support::StrengthLevel strength, const slang::ast::TimingControl* delay,
    WalkFrame frame) -> diag::Result<hir::ContinuousAssign> {
  std::optional<hir::AssignmentDelay> lowered_delay;
  if (delay != nullptr) {
    auto lowered = LowerAssignmentDelay(*this, *delay, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    lowered_delay = *std::move(lowered);
  }
  // LRM 10.3.2: continuous assignment sensitivity is the read set of what
  // drives the target. slang treats the symbol holding it as the procedural
  // scope for analysis purposes.
  const auto& reads = owner_->Sensitivity().AnalyzeReads(driving, containing);
  auto sensitivity = owner_->SensitivityEntriesOf(*this, reads, frame);
  if (!sensitivity) return std::unexpected(std::move(sensitivity.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(std::move(lhs));
  const hir::ExprId rhs_id = frame.Exprs().Add(std::move(rhs));
  return hir::ContinuousAssign{
      .span = span,
      .lhs = lhs_id,
      .rhs = rhs_id,
      .strength = strength,
      .delay = lowered_delay,
      .sensitivity_list = *std::move(sensitivity),
  };
}

auto StructuralScopeLowerer::LowerContinuousAssign(
    const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
    -> diag::Result<hir::ContinuousAssign> {
  const auto span = frontend::PointSpanOf(sym.location);

  auto strength = TranslateDriveStrength(sym.getDriveStrength(), span);
  if (!strength) return std::unexpected(std::move(strength.error()));

  const auto& assignment_expr = sym.getAssignment();
  if (assignment_expr.kind != slang::ast::ExpressionKind::Assignment) {
    // slang bound the continuous assignment to a form other than a plain
    // assignment (a legitimate construct Lyra does not lower yet), not a
    // compiler-invariant violation.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedContinuousAssignForm,
        "this continuous-assignment form is not yet supported");
  }
  const auto& assign = assignment_expr.as<slang::ast::AssignmentExpression>();

  auto lhs_or = LowerExpr(assign.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));

  // LRM 7.5.1 admits `new[]` on the right-hand side of a variable declaration
  // assignment and of a blocking procedural assignment, and a continuous
  // assignment is neither: it states a value its target follows for the whole
  // run, which sizing an array once cannot be.
  if (assign.right().kind == slang::ast::ExpressionKind::NewArray) {
    return diag::Fail(
        frontend::SpanOf(assign.right().sourceRange),
        diag::DiagCode::kUnsupportedContinuousAssignForm,
        "sizing a dynamic array with new[] is not legal in a continuous "
        "assignment (LRM 7.5.1); size it where the array is declared or in a "
        "procedural assignment");
  }

  auto rhs_or = LowerExpr(assign.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));

  return BuildContinuousAssign(
      span, *std::move(lhs_or), *std::move(rhs_or), assignment_expr, sym,
      *strength, sym.getDelay(), frame);
}

}  // namespace lyra::lowering::ast_to_hir
