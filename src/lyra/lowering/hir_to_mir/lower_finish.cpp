#include "lyra/lowering/hir_to_mir/lower_finish.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <variant>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_finish.hpp"
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
  if (c.value_words.empty()) return 0;
  return static_cast<std::int64_t>(c.value_words[0]);
}

}  // namespace

auto LowerFinishSystemSubroutineCall(
    const UnitLoweringState& unit_state, const hir::ProceduralBody& hir_proc,
    const hir::CallExpr& call, const support::SystemSubroutineDesc& desc,
    const support::TerminationSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  int level = info.default_level;
  if (!call.arguments.empty()) {
    const hir::ExprId arg_id = call.arguments.front();
    const hir::Expr& arg_expr = hir_proc.exprs.at(arg_id.value);
    const auto literal = TryExtractLiteralInt(arg_expr);
    if (!literal.has_value()) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          std::format(
              "{} argument must be an integer literal in this build",
              std::string{desc.name}),
          diag::UnsupportedCategory::kFeature);
    }
    if (*literal != 0 && *literal != 1 && *literal != 2) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          std::format("{} argument must be 0, 1, or 2", std::string{desc.name}),
          diag::UnsupportedCategory::kFeature);
    }
    level = static_cast<int>(*literal);
  }
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFinishCall{.level = level}},
      .type = unit_state.Builtins().void_type};
}

}  // namespace lyra::lowering::hir_to_mir
