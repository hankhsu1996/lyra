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
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"
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
    const ProcessLowerer& process, const WalkFrame& frame,
    const hir::CallExpr& call, std::string_view name,
    const support::TerminationSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  int level = info.default_level;
  if (!call.arguments.empty()) {
    if (!call.arguments.front().has_value()) {
      throw InternalError("$finish argument unexpectedly elided");
    }
    const hir::ExprId arg_id = *call.arguments.front();
    const hir::Expr& arg_expr = hir_proc.exprs.Get(arg_id);
    const auto literal = TryExtractLiteralInt(arg_expr);
    if (!literal.has_value()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          std::format(
              "{} argument must be an integer literal", std::string{name}));
    }
    if (*literal != 0 && *literal != 1 && *literal != 2) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          std::format("{} argument must be 0, 1, or 2", std::string{name}));
    }
    level = static_cast<int>(*literal);
  }
  const auto& builtins = process.Owner().Unit().builtins;
  const mir::ExprId runtime_id = frame.current_block->exprs.Add(
      BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId level_id = frame.current_block->exprs.Add(
      mir::MakeIntLiteral(builtins.int_type, static_cast<std::int64_t>(level)));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFinish},
              .arguments = {runtime_id, level_id}},
      .type = builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir
