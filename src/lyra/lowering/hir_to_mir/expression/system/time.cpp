#include "lyra/lowering/hir_to_mir/expression/system/time.hpp"

#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

struct TimeFnInfo {
  support::BuiltinFn id;
  mir::TypeId result_type;
};

auto SelectTimeFn(const mir::BuiltinMirTypes& builtins, support::TimeKind kind)
    -> TimeFnInfo {
  switch (kind) {
    case support::TimeKind::kTime:
      return {.id = support::BuiltinFn::kSimTime, .result_type = builtins.time};
    case support::TimeKind::kStime:
      return {
          .id = support::BuiltinFn::kSTime, .result_type = builtins.int_type};
    case support::TimeKind::kRealtime:
      return {
          .id = support::BuiltinFn::kRealTime,
          .result_type = builtins.realtime};
  }
  throw InternalError("SelectTimeFn: unknown TimeKind");
}

}  // namespace

auto LowerTimeSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    const support::TimeSystemSubroutineInfo& info) -> diag::Result<mir::Expr> {
  const auto& builtins = process.Owner().Unit().builtins;
  const TimeFnInfo fn = SelectTimeFn(builtins, info.kind);
  auto& body = *frame.current_block;
  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(process.Owner(), frame));
  const mir::ExprId unit_power_id = body.exprs.Add(
      mir::MakeIntLiteral(
          builtins.int_type,
          static_cast<std::int64_t>(process.Resolution().unit_power)));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = fn.id},
              .arguments = {services_id, unit_power_id}},
      .type = fn.result_type};
}

}  // namespace lyra::lowering::hir_to_mir
