#include "lyra/lowering/hir_to_mir/module.hpp"

#include <cstdint>
#include <expected>
#include <utility>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/hir_to_mir/routine.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModule(
    const hir::ModuleBody& body, const LoweringInput& input,
    mir::Arena body_arena, const mir::Arena& design_arena,
    const DesignDeclarations& decls, const BodyLocalDecls& body_decls,
    hir::ModuleBodyId body_id,
    mir::ImmediateCoverSiteRegistry* cover_site_registry)
    -> Result<MirBodyLoweringResult> {
  mir::ModuleBody result;

  // Body-local slot descriptors come from specialization-local collection,
  // not from design-global declaration state.
  result.slots = body_decls.slots;

  // Body-local origin storage. All origins produced during body lowering
  // are isolated here, not in a shared origin map.
  OriginMap body_origins;

  // Phase 1: Pre-allocate mir::FunctionIds and build symbol map.
  // Contains only body-local FunctionIds. Package functions are resolved
  // as DesignFunctionRef by ResolveCallTarget.
  SymbolToMirFunctionMap symbol_to_mir_function;

  std::vector<std::pair<hir::FunctionId, mir::FunctionId>> function_pairs;

  for (hir::FunctionId hir_func_id : body.functions) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];

    // Reserve the mir::FunctionId with frozen signature
    mir::FunctionSignature sig = BuildFunctionSignature(
        hir_func, *input.symbol_table, *input.type_arena);
    mir::FunctionId mir_func_id = body_arena.ReserveFunction(std::move(sig));
    symbol_to_mir_function[hir_func.symbol] = mir_func_id;
    function_pairs.emplace_back(hir_func_id, mir_func_id);
    result.functions.push_back(mir_func_id);
  }

  // Phase 2: Lower function bodies (map is complete, recursion works)
  // Module body lowering uses body-local places (kModuleSlot) for module-owned
  // state. Design-global places are created lazily by LookupPlace with
  // explicit kDesignGlobal roots.
  DeclView decl_view{
      .body_places = &body_decls.places,
      .design_places = &decls.design_places,
      .functions = &symbol_to_mir_function,
      .slots = &decls.slots,
      .body_slots = &result.slots,
      .design_arena = &design_arena,
      .design_functions = &decls.functions,
      .dpi_imports = &decls.dpi_imports,
      .cover_site_registry = cover_site_registry};
  for (auto [hir_func_id, mir_func_id] : function_pairs) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];

    Result<mir::Function> mir_func_result = LowerFunctionBody(
        hir_func, input, body_arena, decl_view, &body_origins, body_id);
    if (!mir_func_result) {
      return std::unexpected(mir_func_result.error());
    }
    mir::Function mir_func = std::move(*mir_func_result);

    // Record function and parameter origins
    mir_func.origin = body_origins.Record(mir_func_id, hir_func_id, body_id);

    mir_func.param_origins.reserve(hir_func.parameters.size());
    for (uint32_t i = 0; i < hir_func.parameters.size(); ++i) {
      PrologueParamRef mir_ref{.func = mir_func_id, .param_index = i};
      FunctionParamRef hir_ref{.func = hir_func_id, .param_index = i};
      mir_func.param_origins.push_back(
          body_origins.Record(mir_ref, hir_ref, body_id));
    }

    body_arena.SetFunctionBody(mir_func_id, std::move(mir_func));
  }

  // Phase 3: Lower module processes (can reference functions)
  // Collect dynamically generated functions (e.g., observer programs)
  std::vector<mir::FunctionId> generated_functions;
  for (hir::ProcessId proc_id : body.processes) {
    const hir::Process& hir_process = (*input.hir_arena)[proc_id];
    Result<mir::ProcessId> mir_proc_result = LowerProcess(
        proc_id, hir_process, input, body_arena, decl_view, &body_origins,
        &generated_functions, body_id);
    if (!mir_proc_result) {
      return std::unexpected(mir_proc_result.error());
    }
    result.processes.push_back(*mir_proc_result);
  }

  // Merge generated functions into module's function list
  for (mir::FunctionId func_id : generated_functions) {
    result.functions.push_back(func_id);
  }

  // Note: Tasks are lowered similarly to functions
  for (hir::TaskId task_id : body.tasks) {
    (void)task_id;
  }

  // Move body arena into the result body unit
  result.arena = std::move(body_arena);

  return MirBodyLoweringResult{
      .body = std::move(result),
      .origins = std::move(body_origins).TakeEntries(),
      .symbol_to_function = std::move(symbol_to_mir_function),
  };
}

}  // namespace lyra::lowering::hir_to_mir
