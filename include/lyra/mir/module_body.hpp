#pragma once

#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

enum class SlotKind : uint8_t;
struct SlotDesc;

// Specialization-owned behavioral MIR for a module body.
//
// Owns all body-local MIR storage: processes, functions, places, and
// slot descriptors. ProcessIds, FunctionIds, and body-local PlaceIds are
// indices into the embedded arena. They are permanently body-local.
//
// Invariants:
// - No instance identity (no instance_sym, no instance path strings)
// - No placement/layout ownership (no slot offsets)
// - No assembly-scoped data (no binding/connectivity metadata)
// - No design-global routing metadata
// - All body-local IDs resolve through arena (no rebasing)
struct ModuleBody {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;

  // Body-local slot descriptors, indexed by kModuleSlot id.
  // This is the body's required storage interface: what slots exist,
  // their kinds, and their types. This is NOT placement/layout metadata.
  std::vector<SlotDesc> slots;

  // Body-local MIR storage. All body-local PlaceIds, ProcessIds, and
  // FunctionIds are indices into this arena.
  Arena arena;
};

}  // namespace lyra::mir
