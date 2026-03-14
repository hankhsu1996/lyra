#pragma once

#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::mir {

// MIR optimization stage entry point.
//
// Runs after MIR verification, before MIR consumers (stats, LLVM lowering).
// Operates on verified MIR and produces optimized MIR that preserves all
// MIR invariants. Each pass is routine-local (no interprocedural reasoning).
void RunMirOptimizations(const Design& design, Arena& arena);

}  // namespace lyra::mir
