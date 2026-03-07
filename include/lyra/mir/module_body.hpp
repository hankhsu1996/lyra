#pragma once

#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

enum class SlotKind : uint8_t;
struct SlotDesc;

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

  // Body-local slot descriptors, indexed by kModuleSlot id.
  // This is the body's required storage interface: what slots exist,
  // their kinds, and their types. This is NOT placement/layout metadata.
  std::vector<SlotDesc> slots;
};

}  // namespace lyra::mir
