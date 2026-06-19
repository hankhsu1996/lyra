#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
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

auto LowerPrintTimescaleSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::SystemSubroutineId id) -> diag::Result<mir::Expr> {
  const auto& builtins = process.Module().Unit().builtins;
  auto& body = *frame.current_procedural_scope;
  const auto resolution = process.Resolution();

  const mir::ExprId services_id =
      body.AddExpr(BuildServicesCallExpr(process, frame));
  const mir::ExprId scope_name_id = body.AddExpr(
      mir::Expr{
          .data =
              mir::StringLiteral{.value = std::string(process.Scope().Name())},
          .type = builtins.string});
  const mir::ExprId unit_power_id = body.AddExpr(
      mir::MakeInt32Literal(
          builtins.int32, static_cast<std::int64_t>(resolution.unit_power)));
  const mir::ExprId precision_power_id = body.AddExpr(
      mir::MakeInt32Literal(
          builtins.int32,
          static_cast<std::int64_t>(resolution.precision_power)));

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::SystemSubroutineCallee{.id = id},
              .arguments =
                  {services_id, scope_name_id, unit_power_id,
                   precision_power_id}},
      .type = builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir
