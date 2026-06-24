#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"

#include <cstdint>
#include <expected>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTimeFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& body = *frame.current_block;
  const auto& args = call.arguments;
  if (!args.empty() && args.size() != 4) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedSubroutineArgument,
        "$timeformat takes either no arguments or exactly four (LRM 20.4.3)");
  }

  std::vector<mir::ExprId> call_args;
  call_args.push_back(
      body.exprs.Add(BuildServicesCallExpr(process.Module(), frame)));
  for (const auto& arg : args) {
    if (!arg.has_value()) {
      throw InternalError(
          "$timeformat positional argument unexpectedly elided");
    }
    auto lowered = process.LowerExpr(hir_proc.exprs.Get(*arg), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    call_args.push_back(body.exprs.Add(*std::move(lowered)));
  }

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::SystemSubroutineCallee{.id = id},
              .arguments = std::move(call_args)},
      .type = process.Module().Unit().builtins.void_type};
}

auto LowerPrintTimescaleSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::SystemSubroutineId id) -> diag::Result<mir::Expr> {
  const auto& builtins = process.Module().Unit().builtins;
  auto& body = *frame.current_block;
  const auto resolution = process.Resolution();

  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(process.Module(), frame));
  const mir::ExprId scope_name_lit = body.exprs.Add(
      mir::Expr{
          .data =
              mir::StringLiteral{.value = std::string(process.Owner().Name())},
          .type = builtins.string});
  const mir::ExprId scope_name_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::ConstructorCallee{},
                  .arguments = {scope_name_lit}},
          .type = builtins.string});
  const mir::ExprId unit_power_id = body.exprs.Add(
      mir::MakeInt32Literal(
          builtins.int32, static_cast<std::int64_t>(resolution.unit_power)));
  const mir::ExprId precision_power_id = body.exprs.Add(
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
