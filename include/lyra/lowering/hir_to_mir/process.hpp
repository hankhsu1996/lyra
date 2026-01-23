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
auto LowerProcess(
    const hir::Process& process, const LoweringInput& input,
    mir::Arena& mir_arena, const DeclView& decl_view, OriginMap* origin_map)
    -> mir::ProcessId;

}  // namespace lyra::lowering::hir_to_mir
