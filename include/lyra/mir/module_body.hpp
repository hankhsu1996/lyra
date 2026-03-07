#pragma once

#include <vector>

#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// Specialization-owned behavioral MIR for a module body.
//
// This owns only the behavioral IR that will eventually be shared across
// instances of the same specialization. The mapping is currently 1:1 with
// instances, but the ownership model is correct for future sharing.
//
// Invariants:
// - No instance identity (no instance_sym, no instance path strings)
// - No placement/layout ownership (no slot offsets)
// - No assembly-scoped data (no binding/connectivity metadata)
// - No design-global routing metadata
struct ModuleBody {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
};

}  // namespace lyra::mir
