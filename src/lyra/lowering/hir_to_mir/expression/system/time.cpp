#include "lyra/lowering/hir_to_mir/expression/system/time.hpp"

#include <cstdint>

#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTimeSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::SystemSubroutineId id,
    const support::TimeSystemSubroutineInfo& info) -> diag::Result<mir::Expr> {
  const auto& builtins = process.Module().Unit().builtins;
  mir::TypeId result_type = builtins.time;
  switch (info.kind) {
    case support::TimeKind::kTime:
      result_type = builtins.time;
      break;
    case support::TimeKind::kStime:
      result_type = builtins.int32;
      break;
    case support::TimeKind::kRealtime:
      result_type = builtins.realtime;
      break;
  }
  auto& body = *frame.current_block;
  const mir::ExprId services_id =
      body.AddExpr(BuildServicesCallExpr(process, frame));
  const mir::ExprId unit_power_id = body.AddExpr(
      mir::MakeInt32Literal(
          builtins.int32,
          static_cast<std::int64_t>(process.Resolution().unit_power)));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::SystemSubroutineCallee{.id = id},
              .arguments = {services_id, unit_power_id}},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
