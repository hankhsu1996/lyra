#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"

#include <array>
#include <cstddef>
#include <expected>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_timescale.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTimeFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  const auto& args = call.arguments;
  std::optional<mir::TimeFormatArgExprs> arg_exprs;
  if (!args.empty()) {
    if (args.size() != 4) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedSubroutineArgument,
          "$timeformat takes either no arguments or exactly four (LRM 20.4.3)",
          diag::UnsupportedCategory::kFeature);
    }
    std::array<mir::ExprId, 4> ids{};
    for (std::size_t i = 0; i < args.size(); ++i) {
      if (!args[i].has_value()) {
        throw InternalError(
            "$timeformat positional argument unexpectedly elided");
      }
      auto lowered =
          process.LowerExpr(hir_proc.exprs.at(args[i]->value), frame);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      ids.at(i) = proc_scope.AddExpr(*std::move(lowered));
    }
    arg_exprs = mir::TimeFormatArgExprs{
        .units = ids[0],
        .precision = ids[1],
        .suffix = ids[2],
        .min_width = ids[3]};
  }
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimeSetTimeFormatCall{.args = std::move(arg_exprs)}},
      .type = process.Module().Unit().builtins.void_type};
}

auto LowerPrintTimescaleSystemSubroutineCall(const ProcessLowerer& process)
    -> diag::Result<mir::Expr> {
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimePrintTimescaleCall{
                      .scope_name = process.Scope().Name()}},
      .type = process.Module().Unit().builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir
