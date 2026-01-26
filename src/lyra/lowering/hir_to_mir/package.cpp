#include "lyra/lowering/hir_to_mir/package.hpp"

#include <cstdint>
#include <utility>

#include "lyra/hir/fwd.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/routine.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerPackage(
    const hir::Package& package, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map,
    const DesignDeclarations& decls) -> mir::Package {
  mir::Package result;

  // Lower function bodies (IDs were pre-allocated in CollectDeclarations)
  DeclView decl_view{
      .places = &decls.design_places, .functions = &decls.functions};
  for (hir::FunctionId hir_func_id : package.functions) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];
    mir::FunctionId mir_func_id = decls.functions.at(hir_func.symbol);

    mir::Function mir_func =
        LowerFunctionBody(hir_func, input, mir_arena, decl_view, origin_map);

    // Record function and parameter origins (caller has both IDs)
    if (origin_map != nullptr) {
      mir_func.origin = origin_map->Record(mir_func_id, hir_func_id);

      // Record per-parameter origins for prologue error reporting
      mir_func.param_origins.reserve(hir_func.parameters.size());
      for (uint32_t i = 0; i < hir_func.parameters.size(); ++i) {
        PrologueParamRef mir_ref{.func = mir_func_id, .param_index = i};
        FunctionParamRef hir_ref{.func = hir_func_id, .param_index = i};
        mir_func.param_origins.push_back(origin_map->Record(mir_ref, hir_ref));
      }
    }

    mir_arena.SetFunctionBody(mir_func_id, std::move(mir_func));
    result.functions.push_back(mir_func_id);
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
