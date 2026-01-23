#include "lyra/lowering/hir_to_mir/module.hpp"

#include <utility>
#include <vector>

#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/hir_to_mir/routine.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModule(
    const hir::Module& module, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map, const PlaceMap& design_places,
    const SymbolToMirFunctionMap& design_functions) -> mir::Module {
  mir::Module result;

  // Phase 1: Pre-allocate mir::FunctionIds and build symbol map
  // Start with design-wide functions (package functions) so module code can
  // call them.
  SymbolToMirFunctionMap symbol_to_mir_function = design_functions;
  std::vector<std::pair<hir::FunctionId, mir::FunctionId>> function_pairs;

  for (hir::FunctionId hir_func_id : module.functions) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];

    // Reserve the mir::FunctionId with frozen signature
    mir::FunctionSignature sig =
        BuildFunctionSignature(hir_func, *input.symbol_table);
    mir::FunctionId mir_func_id = mir_arena.ReserveFunction(std::move(sig));
    symbol_to_mir_function[hir_func.symbol] = mir_func_id;
    function_pairs.emplace_back(hir_func_id, mir_func_id);
    result.functions.push_back(mir_func_id);
  }

  // Phase 2: Lower function bodies (map is complete, recursion works)
  for (auto [hir_func_id, mir_func_id] : function_pairs) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];

    mir::Function mir_func = LowerFunctionBody(
        hir_func, input, mir_arena, design_places, symbol_to_mir_function,
        origin_map);

    mir_arena.SetFunctionBody(mir_func_id, std::move(mir_func));
  }

  // Phase 3: Lower module processes (can reference functions)
  for (hir::ProcessId proc_id : module.processes) {
    const hir::Process& hir_process = (*input.hir_arena)[proc_id];
    mir::ProcessId mir_proc_id = LowerProcess(
        hir_process, input, mir_arena, design_places, symbol_to_mir_function,
        origin_map);
    result.processes.push_back(mir_proc_id);
  }

  // Note: Tasks are lowered similarly to functions
  for (hir::TaskId task_id : module.tasks) {
    (void)task_id;
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
