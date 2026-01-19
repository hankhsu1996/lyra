#pragma once

#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a process with module places for module-level variables.
// module_places is read-only; the process creates its own local_places.
// symbol_to_mir_function enables calling user functions from processes.
auto LowerProcess(
    const hir::Process& process, const LoweringInput& input,
    mir::Arena& mir_arena, const PlaceMap& module_places,
    const SymbolToMirFunctionMap& symbol_to_mir_function) -> mir::ProcessId;

}  // namespace lyra::lowering::hir_to_mir
