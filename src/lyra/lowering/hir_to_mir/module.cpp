#include "lyra/lowering/hir_to_mir/module.hpp"

#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModule(
    const hir::Module& module, const LoweringInput& input,
    mir::Arena& mir_arena) -> mir::Module {
  mir::Module result;

  // Allocate storage for module-level variables
  PlaceMap module_places;
  int next_module_slot = 0;

  for (SymbolId var_sym : module.variables) {
    const Symbol& sym = input.symbol_table[var_sym];
    mir::Place place{
        .root =
            mir::PlaceRoot{
                .kind = mir::PlaceRoot::Kind::kDesign,
                .id = next_module_slot++,
                .type = sym.type,
            },
        .projections = {},
    };
    mir::PlaceId place_id = mir_arena.AddPlace(std::move(place));
    module_places[var_sym] = place_id;
  }

  // Lower processes (each creates its own Context for local/temp allocation)
  for (hir::ProcessId proc_id : module.processes) {
    const hir::Process& hir_process = input.hir_arena[proc_id];
    mir::ProcessId mir_proc_id =
        LowerProcess(hir_process, input, mir_arena, module_places);
    result.processes.push_back(mir_proc_id);
  }

  for (hir::FunctionId func_id : module.functions) {
    // TODO(hankhsu): Lower function (creates Context internally)
    (void)func_id;
  }

  // Note: Tasks are lowered similarly to functions
  for (hir::TaskId task_id : module.tasks) {
    (void)task_id;
  }

  result.num_module_slots = static_cast<size_t>(next_module_slot);
  return result;
}

}  // namespace lyra::lowering::hir_to_mir
