#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto TryExtractLiteralInt(const hir::Expr& expr)
    -> std::optional<std::int64_t> {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  if (primary == nullptr) return std::nullopt;
  const auto* lit = std::get_if<hir::IntegerLiteral>(&primary->data);
  if (lit == nullptr) return std::nullopt;
  const auto& c = lit->value;
  if (c.state_kind == hir::IntegralStateKind::kFourState) return std::nullopt;
  return static_cast<std::int64_t>(c.value_words[0]);
}

}  // namespace

auto LowerDiagnosticLevel(
    const hir::Expr& level, std::string_view argument, diag::SourceSpan span)
    -> diag::Result<int> {
  const auto literal = TryExtractLiteralInt(level);
  if (!literal.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        std::format("{} must be an integer literal", argument));
  }
  if (*literal != 0 && *literal != 1 && *literal != 2) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        std::format("{} must be 0, 1, or 2", argument));
  }
  return static_cast<int>(*literal);
}

auto LowerTerminationSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    const hir::CallExpr& call, std::string_view name,
    const support::TerminationSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  int level = info.default_level;
  if (!call.arguments.empty()) {
    if (!call.arguments.front().has_value()) {
      throw InternalError(
          std::format("{} argument unexpectedly elided", std::string{name}));
    }
    auto level_or = LowerDiagnosticLevel(
        process.HirBody().exprs.Get(*call.arguments.front()),
        std::format("{} argument", name), span);
    if (!level_or) return std::unexpected(std::move(level_or.error()));
    level = *level_or;
  }
  const auto& unit = process.Owner().Unit();
  auto& block = *frame.current_block;
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId origin_id = BuildStringValueExpr(
      unit, block,
      FormatRuntimeOriginString(span, process.Owner().SourceManager()));
  const mir::ExprId level_id =
      BuildIntLiteral(unit, block, static_cast<std::int64_t>(level));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = info.builtin_fn},
              .arguments = {runtime_id, origin_id, level_id}},
      .type = unit.builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir
