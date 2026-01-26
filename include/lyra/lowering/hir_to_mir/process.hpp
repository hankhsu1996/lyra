#pragma once

#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a process with design-level declarations.
// decl_view provides read-only access to design places and functions.
// hir_proc_id is the HIR process ID, used to record the origin mapping.
// generated_functions: optional sink for dynamically created functions
// (thunks).
auto LowerProcess(
    hir::ProcessId hir_proc_id, const hir::Process& process,
    const LoweringInput& input, mir::Arena& mir_arena,
    const DeclView& decl_view, OriginMap* origin_map,
    std::vector<mir::FunctionId>* generated_functions = nullptr)
    -> mir::ProcessId;

}  // namespace lyra::lowering::hir_to_mir
