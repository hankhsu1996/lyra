#pragma once

#include <cstdio>

#include "llvm_stats.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::driver {

// Print process-level statistics.
// Reconstructs the same process ordering as BuildLayout (init, connection,
// module non-final) and correlates with LLVM per-function stats.
void PrintProcessStats(
    const mir::Design& design, const mir::Arena& arena,
    const lowering::OriginMap& origin_map, const hir::Arena& hir_arena,
    const SourceManager& source_manager, const LlvmStats& llvm_stats,
    FILE* sink = stderr);

}  // namespace lyra::driver
