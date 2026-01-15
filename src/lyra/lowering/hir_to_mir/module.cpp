#include "lyra/lowering/hir_to_mir/module.hpp"

#include "lyra/lowering/hir_to_mir/process.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModule(
    const hir::Module& module, const LoweringInput& input,
    mir::Arena& mir_arena) -> mir::Module {
  mir::Module result;

  for (hir::ProcessId proc_id : module.processes) {
    const hir::Process& hir_process = input.hir_arena[proc_id];
    mir::ProcessId mir_proc_id = LowerProcess(hir_process, input, mir_arena);
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

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
