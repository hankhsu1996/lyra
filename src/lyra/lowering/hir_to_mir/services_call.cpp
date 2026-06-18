#include "lyra/lowering/hir_to_mir/services_call.hpp"

#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildServicesCallExpr(
    const ProcessLowerer& process, const WalkFrame& frame) -> mir::Expr {
  auto& body = *frame.current_procedural_scope;
  const auto& builtins = process.Module().Unit().builtins;
  const mir::ExprId self_id =
      body.AddExpr(BuildSelfRefExpr(frame, builtins.self_pointer));
  return mir::MakeServicesCallExpr(self_id, builtins.services);
}

}  // namespace lyra::lowering::hir_to_mir
