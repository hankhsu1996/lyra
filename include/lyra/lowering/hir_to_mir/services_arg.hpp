#pragma once

#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds the `self.Services()` engine-handle argument: a `ProceduralVarRef` to
// `self` (mir.md invariant 11) wrapped in a `ScopeMethod{kServices}` call.
// Lowering sites that emit a runtime-effect or wrapper-API call thread the
// returned id as the call's services argument. See
// `docs/decisions/runtime-effects-as-generic-calls.md`.
[[nodiscard]] inline auto BuildServicesArg(
    const ProcessLowerer& process, const WalkFrame& frame) -> mir::ExprId {
  auto& body = *frame.current_procedural_scope;
  const auto& builtins = process.Module().Unit().builtins;
  const mir::ExprId self_id = body.AddExpr(
      mir::MakeProceduralVarRefExpr(
          frame.procedural_depth - frame.self_decl_depth, *frame.self_binding,
          builtins.self_pointer));
  return body.AddExpr(mir::MakeServicesCallExpr(self_id, builtins.services));
}

}  // namespace lyra::lowering::hir_to_mir
