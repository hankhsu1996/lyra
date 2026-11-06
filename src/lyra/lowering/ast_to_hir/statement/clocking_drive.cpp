#include "lyra/lowering/ast_to_hir/statement/clocking_drive.hpp"

#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/clocking_block.hpp"
#include "lyra/lowering/ast_to_hir/expression/clocking_reach.hpp"
#include "lyra/lowering/ast_to_hir/expression/drive_target.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "slang/ast/TimingControl.h"
#include "slang/ast/expressions/AssignmentExpressions.h"
#include "slang/ast/expressions/MiscExpressions.h"
#include "slang/ast/expressions/SelectExpressions.h"
#include "slang/ast/symbols/VariableSymbols.h"

namespace lyra::lowering::ast_to_hir {

namespace {

// The operand naming the clockvar under whatever selects a drive's target
// wrote -- a bit-select or a slice (LRM 14.16), or a member of a structure the
// clockvar holds, which the grammar's select admits as well -- and whether a
// member is among them.
struct ClockvarUnderSelects {
  const slang::ast::Expression* operand = nullptr;
  bool selects_a_member = false;
};

auto ClockvarUnderSelectsOf(const slang::ast::Expression& target)
    -> ClockvarUnderSelects {
  if (const auto* sel = target.as_if<slang::ast::ElementSelectExpression>()) {
    return ClockvarUnderSelectsOf(sel->value());
  }
  if (const auto* sel = target.as_if<slang::ast::RangeSelectExpression>()) {
    return ClockvarUnderSelectsOf(sel->value());
  }
  if (const auto* access = target.as_if<slang::ast::MemberAccessExpression>();
      access != nullptr &&
      access->member.kind == slang::ast::SymbolKind::Field) {
    return ClockvarUnderSelects{
        .operand = ClockvarUnderSelectsOf(access->value()).operand,
        .selects_a_member = true};
  }
  return ClockvarUnderSelects{.operand = &target, .selects_a_member = false};
}

// The part of a lowered drive target `target` that names the output's driving
// variable, under the selects over it.
auto WrittenPartOf(
    const base::Arena<hir::Expr, hir::ExprId>& exprs, hir::ExprId target)
    -> hir::ExprId {
  const hir::Expr& expr = exprs.Get(target);
  if (const auto* sel = std::get_if<hir::ElementSelectExpr>(&expr.data)) {
    return WrittenPartOf(exprs, sel->base_value);
  }
  if (const auto* range = std::get_if<hir::RangeSelectExpr>(&expr.data)) {
    return WrittenPartOf(exprs, range->base_value);
  }
  return target;
}

auto ClockvarNamedBy(const slang::ast::Expression& operand)
    -> const slang::ast::ClockVarSymbol* {
  const slang::ast::Symbol* symbol = nullptr;
  if (const auto* named = operand.as_if<slang::ast::ValueExpressionBase>()) {
    symbol = &named->symbol;
  } else if (
      const auto* access =
          operand.as_if<slang::ast::MemberAccessExpression>()) {
    symbol = &access->member;
  }
  if (symbol == nullptr) return nullptr;
  return symbol->as_if<slang::ast::ClockVarSymbol>();
}

}  // namespace

auto IsClockingDrive(const slang::ast::AssignmentExpression& as) -> bool {
  if (!as.isNonBlocking()) return false;
  const slang::ast::ClockVarSymbol* clockvar =
      ClockvarNamedBy(*ClockvarUnderSelectsOf(as.left()).operand);
  return clockvar != nullptr && IsClockingOutput(*clockvar);
}

auto LowerClockingDrive(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  UnitLowerer& unit_lowerer = proc.Owner();
  const ClockvarUnderSelects under = ClockvarUnderSelectsOf(as.left());
  const slang::ast::Expression& operand = *under.operand;
  const slang::ast::ClockVarSymbol& clockvar = *ClockvarNamedBy(operand);
  const std::optional<ClockingNaming> naming =
      ClockingNamingOf(unit_lowerer, frame, operand);
  if (!naming.has_value()) {
    throw InternalError(
        "LowerClockingDrive: a name standing for a clockvar names its block");
  }
  if (const auto refused = OutputRefusal(clockvar)) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm, *refused);
  }
  if (under.selects_a_member) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a drive of a member of a clocking output is not yet supported");
  }
  auto type = unit_lowerer.InternType(clockvar.getType(), span);
  if (!type) return std::unexpected(std::move(type.error()));

  // The drive writes the output's driving variable, reached as the block is,
  // under the selects the drive wrote (LRM 14.16).
  auto written = LowerClockingPart(
      proc, frame, *naming, ClockingDrivingVariable{.clockvar = &clockvar},
      *type, span);
  if (!written) return std::unexpected(std::move(written.error()));
  DriveTargetLowerer targets(proc, operand, *std::move(written));
  auto target = targets.LowerExpr(as.left(), frame);
  if (!target) return std::unexpected(std::move(target.error()));
  const hir::ExprId lhs = frame.Exprs().Add(*std::move(target));
  auto landing = LandingOf(
      unit_lowerer, frame, *naming, clockvar, WrittenPartOf(frame.Exprs(), lhs),
      span);
  if (!landing) return std::unexpected(std::move(landing.error()));

  std::optional<hir::ExprId> cycles;
  if (as.timingControl != nullptr) {
    const auto* cycle_delay =
        as.timingControl->as_if<slang::ast::CycleDelayControl>();
    if (cycle_delay == nullptr) {
      throw InternalError(
          "LowerClockingDrive: the front end admits only a cycle delay in a "
          "synchronous drive");
    }
    auto count = proc.LowerExpr(cycle_delay->expr, frame);
    if (!count) return std::unexpected(std::move(count.error()));
    cycles = frame.Exprs().Add(*std::move(count));
  }
  auto value = proc.LowerExpr(as.right(), frame);
  if (!value) return std::unexpected(std::move(value.error()));
  const hir::ExprId assignment = frame.Exprs().Add(
      hir::Expr{
          .type = frame.Exprs().Get(lhs).type,
          .data =
              hir::AssignExpr{
                  .timing =
                      hir::NonBlockingEffect{
                          .control =
                              hir::ClockingDriveControl{
                                  .cycles = cycles,
                                  .landing = *std::move(landing)}},
                  .lhs = lhs,
                  .compound = std::nullopt,
                  .rhs = frame.Exprs().Add(*std::move(value))},
          .span = span});
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::ExprStmt{.expr = assignment},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
