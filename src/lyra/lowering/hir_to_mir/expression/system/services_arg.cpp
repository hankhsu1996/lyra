#include "lyra/lowering/hir_to_mir/expression/system/services_arg.hpp"

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildServicesArg(const ProcessLowerer& process, const WalkFrame& frame)
    -> mir::ExprId {
  auto& body = *frame.current_procedural_scope;
  const auto& builtins = process.Module().Unit().builtins;
  const mir::ExprId self_id = body.AddExpr(
      mir::MakeProceduralVarRefExpr(
          frame.procedural_depth - frame.self_decl_depth, *frame.self_binding,
          builtins.self_pointer));
  return body.AddExpr(mir::MakeServicesCallExpr(self_id, builtins.services));
}

}  // namespace lyra::lowering::hir_to_mir
