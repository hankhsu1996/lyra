#include "lyra/lowering/hir_to_mir/package.hpp"

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
    mir::Arena& mir_arena, OriginMap* origin_map, const PlaceMap& design_places,
    const SymbolToMirFunctionMap& design_functions) -> mir::Package {
  mir::Package result;

  // Lower function bodies (IDs were pre-allocated in design-level Phase 1)
  for (hir::FunctionId hir_func_id : package.functions) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];
    mir::FunctionId mir_func_id = design_functions.at(hir_func.symbol);

    mir::Function mir_func = LowerFunctionBody(
        hir_func, input, mir_arena, design_places, design_functions,
        origin_map);
    mir_arena.SetFunctionBody(mir_func_id, std::move(mir_func));
    result.functions.push_back(mir_func_id);
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
