#include "lyra/lowering/hir_to_mir/expression/system/time.hpp"

#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto LowerTimeSystemSubroutineCall(
    const Lowerer& lowerer, const WalkFrame& frame,
    const support::TimeSystemSubroutineInfo& info) -> diag::Result<mir::Expr> {
  const auto& builtins = lowerer.Owner().Unit().builtins;
  auto& body = *frame.current_block;
  const mir::ExprId runtime_id =
      body.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  const mir::ExprId unit_power_id = BuildMachineIntLiteral(
      lowerer.Owner().Unit(), body,
      static_cast<std::int64_t>(lowerer.Resolution().unit_power));
  const auto call = [&](support::BuiltinFn entry, mir::TypeId result) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = mir::Direct{.target = entry},
                .arguments = {runtime_id, unit_power_id}},
        .type = result};
  };
  switch (info.kind) {
    case support::TimeKind::kTime:
      return call(support::BuiltinFn::kSimTime, builtins.time);
    case support::TimeKind::kStime:
      return call(support::BuiltinFn::kSTime, builtins.int_unsigned);
    case support::TimeKind::kRealtime:
      return call(support::BuiltinFn::kRealTime, builtins.real);
  }
  throw InternalError("LowerTimeSystemSubroutineCall: unknown TimeKind");
}

template auto LowerTimeSystemSubroutineCall(
    const ProcessLowerer&, const WalkFrame&,
    const support::TimeSystemSubroutineInfo&) -> diag::Result<mir::Expr>;
template auto LowerTimeSystemSubroutineCall(
    const StructuralScopeLowerer&, const WalkFrame&,
    const support::TimeSystemSubroutineInfo&) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
