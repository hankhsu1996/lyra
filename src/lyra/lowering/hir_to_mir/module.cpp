#include "lyra/lowering/hir_to_mir/module.hpp"

#include <cstdint>
#include <expected>
#include <unordered_set>
#include <utility>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/decision_site_allocator.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/hir_to_mir/routine.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModule(
    const hir::ModuleBody& body, const LoweringInput& input,
    mir::Arena body_arena, const mir::Arena& design_arena,
    const DesignDeclarations& decls, const BodyLocalDecls& body_decls,
    hir::ModuleBodyId body_id,
    mir::ImmediateCoverSiteRegistry* cover_site_registry,
    mir::DeferredAssertionSiteRegistry* deferred_assertion_site_registry,
    absl::flat_hash_map<
        mir::DeferredAssertionActionKey, mir::DeferredUserCallRealization>*
        deferred_assertion_realizations,
    const PlaceMap* cross_instance_places) -> Result<MirBodyLoweringResult> {
  mir::ModuleBody result;

  // Body-local slot descriptors come from specialization-local collection,
  // not from design-global declaration state.
  result.slots = body_decls.slots;

  // Body-local origin storage. All origins produced during body lowering
  // are isolated here, not in a shared origin map.
  OriginMap body_origins;

  // B2: External ref registry for body lowering.
  // Shared across all processes/functions in this body.
  std::vector<mir::ExternalAccessRecipe> external_refs;
  std::vector<ProvisionalNonLocalTarget> provisional_targets;

  // Phase 1: Pre-allocate mir::FunctionIds and build symbol map.
  // Contains only body-local FunctionIds. Package functions are resolved
  // as DesignFunctionRef by ResolveCallTarget.
  SymbolToMirFunctionMap symbol_to_mir_function;

  // Collect task symbols admitted by validated module-scoped DPI exports.
  // Only these tasks are lowered as immediate callables for export wrappers.
  std::unordered_set<SymbolId, SymbolIdHash> admitted_export_task_symbols;
  for (const auto& exp : body.dpi_exports) {
    if (exp.is_task && exp.is_module_scoped) {
      admitted_export_task_symbols.insert(exp.symbol);
    }
  }

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
      .cross_instance_places =
          cross_instance_places,  // V3d residual: design-level paths only
      .functions = &symbol_to_mir_function,
      .slots = &decls.slots,
      .body_slots = &result.slots,
      .design_arena = &design_arena,
      .design_functions = &decls.functions,
      .dpi_imports = &decls.dpi_imports,
      .cover_site_registry = cover_site_registry,
      .deferred_assertion_site_registry = deferred_assertion_site_registry,
      .deferred_assertion_realizations = deferred_assertion_realizations,
      .external_refs = &external_refs,
      .provisional_targets = &provisional_targets};

  // Body-global decision site allocator: all processes, functions, and tasks
  // within this module body share one allocator so decision IDs are unique
  // across the body. This ensures function-body decision IDs do not collide
  // with process-body IDs in the owner's runtime table.
  DecisionSiteAllocator body_decision_allocator;
  for (auto [hir_func_id, mir_func_id] : function_pairs) {
    const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];

    Result<mir::Function> mir_func_result = LowerFunctionBody(
        hir_func, input, body_arena, decl_view, &body_origins, body_id,
        &body_decision_allocator);
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
        &generated_functions, body_id, &body_decision_allocator);
    if (!mir_proc_result) {
      return std::unexpected(mir_proc_result.error());
    }
    result.processes.push_back(*mir_proc_result);
  }

  // Merge generated functions into module's function list
  for (mir::FunctionId func_id : generated_functions) {
    result.functions.push_back(func_id);
  }

  // Lower admitted export task bodies as immediate callables.
  // Only tasks in the admitted set (validated non-suspending DPI exports)
  // are materialized. Task semantic identity is preserved at the export
  // declaration/wrapper layer; lowering to mir::Function is the immediate
  // internal callable representation only.
  std::vector<std::pair<hir::TaskId, mir::FunctionId>> task_pairs;
  for (hir::TaskId hir_task_id : body.tasks) {
    const hir::Task& hir_task = (*input.hir_arena)[hir_task_id];
    if (!admitted_export_task_symbols.contains(hir_task.symbol)) continue;
    mir::FunctionSignature sig = BuildTaskSignature(
        hir_task, *input.symbol_table, input.builtin_types.void_type);
    mir::FunctionId mir_func_id = body_arena.ReserveFunction(std::move(sig));
    symbol_to_mir_function[hir_task.symbol] = mir_func_id;
    task_pairs.emplace_back(hir_task_id, mir_func_id);
    result.functions.push_back(mir_func_id);
  }
  for (auto [hir_task_id, mir_func_id] : task_pairs) {
    const hir::Task& hir_task = (*input.hir_arena)[hir_task_id];
    Result<mir::Function> mir_func_result = LowerTaskBody(
        hir_task, input, body_arena, decl_view, &body_origins, body_id,
        &body_decision_allocator);
    if (!mir_func_result) {
      return std::unexpected(mir_func_result.error());
    }
    mir::Function mir_func = std::move(*mir_func_result);
    mir_func.origin = body_origins.Record(mir_func_id, hir_task_id, body_id);
    mir_func.param_origins.reserve(hir_task.parameters.size());
    for (uint32_t i = 0; i < hir_task.parameters.size(); ++i) {
      PrologueParamRef mir_ref{.func = mir_func_id, .param_index = i};
      TaskParamRef hir_ref{.task = hir_task_id, .param_index = i};
      mir_func.param_origins.push_back(
          body_origins.Record(mir_ref, hir_ref, body_id));
    }
    body_arena.SetFunctionBody(mir_func_id, std::move(mir_func));
  }

  // Record the body-global decision site count. The LLVM backend validates
  // that exactly this many sites are reconstructed from the stored records.
  result.total_decision_sites = body_decision_allocator.TotalAllocated();

  // Propagate decision owner acceptance through internal call graph.
  // body_requirement stays intrinsic; only abi_contract is propagated.
  // Pass design_arena for cross-arena DesignFunctionRef edge resolution.
  body_arena.PropagateDeferredOwnerAbi(&design_arena);

  // Move body arena into the result body unit
  result.arena = std::move(body_arena);

  return MirBodyLoweringResult{
      .body = std::move(result),
      .origins = std::move(body_origins).TakeEntries(),
      .symbol_to_function = std::move(symbol_to_mir_function),
      .external_refs = std::move(external_refs),
      .provisional_targets = std::move(provisional_targets),
  };
}

}  // namespace lyra::lowering::hir_to_mir
