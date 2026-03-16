#include "lyra/mir/optimize.hpp"

#include "lyra/mir/passes/bounds_check_elimination.hpp"

namespace lyra::mir {

// MIR optimization stage sequencing point.
// Each pass is added here as an explicit call. Do not use a generic pass
// manager; keep the sequencing readable and the dependency on each pass
// visible at the call site.
void RunMirOptimizations(Design& design, Arena& design_arena) {
  passes::EliminateRedundantBoundsChecks(design, design_arena);
}

}  // namespace lyra::mir
