#pragma once

#include <cstdio>

#include "llvm_stats.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::driver {

// Print process-level statistics.
// Reconstructs the same process ordering as BuildLayout (init, connection,
// module non-final) and correlates with LLVM per-function stats.
//
// Takes the OriginMap and HIR storage domain references directly.
// Span resolution uses lowering::ResolveHirArena internally.
void PrintProcessStats(
    const mir::Design& design, const mir::Arena& design_arena,
    const lowering::OriginMap& design_origins, const hir::Design& hir_design,
    const hir::Arena& global_hir_arena, const SourceManager& source_manager,
    const LlvmStats& llvm_stats, FILE* sink = stderr);

}  // namespace lyra::driver
