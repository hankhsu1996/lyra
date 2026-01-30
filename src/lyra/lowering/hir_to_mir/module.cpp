#include "lyra/lowering/hir_to_mir/module.hpp"

#include <cstdint>
#include <expected>
#include <utility>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
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
    mir::Arena& mir_arena, OriginMap* origin_map,
    const DesignDeclarations& decls) -> Result<mir::Module> {
  mir::Module result;
  result.instance_sym = module.symbol;

  // Phase 1: Pre-allocate mir::FunctionIds and build symbol map
  // Start with design-wide functions (package functions) so module code can
  // call them.
  SymbolToMirFunctionMap symbol_to_mir_function = decls.functions;
  std::vector<std::pair<hir::FunctionId, mir::FunctionId>> function_pairs;

  for (hir::FunctionId hir_func_id : module.functions) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];

    // Reserve the mir::FunctionId with frozen signature
    mir::FunctionSignature sig = BuildFunctionSignature(
        hir_func, *input.symbol_table, *input.type_arena);
    mir::FunctionId mir_func_id = mir_arena.ReserveFunction(std::move(sig));
    symbol_to_mir_function[hir_func.symbol] = mir_func_id;
    function_pairs.emplace_back(hir_func_id, mir_func_id);
    result.functions.push_back(mir_func_id);
  }

  // Phase 2: Lower function bodies (map is complete, recursion works)
  DeclView decl_view{
      .places = &decls.design_places, .functions = &symbol_to_mir_function};
  for (auto [hir_func_id, mir_func_id] : function_pairs) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];

    Result<mir::Function> mir_func_result =
        LowerFunctionBody(hir_func, input, mir_arena, decl_view, origin_map);
    if (!mir_func_result) {
      return std::unexpected(mir_func_result.error());
    }
    mir::Function mir_func = std::move(*mir_func_result);

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
  }

  // Phase 3: Lower module processes (can reference functions)
  // Look up instance index for %m support
  uint32_t owner_instance_id = UINT32_MAX;
  auto it = decls.instance_indices.find(module.symbol);
  if (it != decls.instance_indices.end()) {
    owner_instance_id = it->second;
  }

  // Collect dynamically generated functions (e.g., strobe thunks)
  std::vector<mir::FunctionId> generated_functions;
  for (hir::ProcessId proc_id : module.processes) {
    const hir::Process& hir_process = (*input.hir_arena)[proc_id];
    Result<mir::ProcessId> mir_proc_result = LowerProcess(
        proc_id, hir_process, input, mir_arena, decl_view, origin_map,
        owner_instance_id, &generated_functions);
    if (!mir_proc_result) {
      return std::unexpected(mir_proc_result.error());
    }
    result.processes.push_back(*mir_proc_result);
  }

  // Merge generated functions (thunks) into module's function list
  for (mir::FunctionId func_id : generated_functions) {
    result.functions.push_back(func_id);
  }

  // Note: Tasks are lowered similarly to functions
  for (hir::TaskId task_id : module.tasks) {
    (void)task_id;
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
