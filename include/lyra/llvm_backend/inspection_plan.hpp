#pragma once

#include <span>
#include <string>
#include <vector>

#include "lyra/common/byte_offset.hpp"
#include "lyra/common/slot_id.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/storage_types.hpp"

namespace lyra::lowering::mir_to_llvm {

// Pre-layout identity record. Test framework produces these.
// The backend inspection planner resolves them into typed placements.
struct InspectedVarRef {
  std::string name;
  common::SlotId slot_id;
};

// Typed placement for a design-global variable.
// Byte address = design_state + abs_off.
struct DesignGlobalPlacement {
  ArenaByteOffset abs_off;
};

// Typed placement for an instance-owned variable.
// Byte address resolved at runtime from RuntimeInstance::storage + rel_off.
struct InstanceOwnedPlacement {
  uint32_t owner_instance_id;
  common::InstanceByteOffset rel_off;
};

// Concrete per-variable record for design-global inspection.
struct InspectedGlobalVar {
  std::string name;
  DesignGlobalPlacement placement;
  SlotTypeInfo type_info;
};

// Concrete per-variable record for instance-owned inspection.
struct InspectedInstanceVar {
  std::string name;
  InstanceOwnedPlacement placement;
  SlotTypeInfo type_info;
};

// Typed inspection plan artifact. Pre-partitioned by storage domain
// with concrete record types per bucket. The plan structure itself
// enforces the domain split -- no variant, no double-encoding.
struct InspectionPlan {
  std::vector<InspectedGlobalVar> globals;
  std::vector<InspectedInstanceVar> instance_owned;

  [[nodiscard]] auto IsEmpty() const -> bool {
    return globals.empty() && instance_owned.empty();
  }
};

// Build an inspection plan from layout-owned artifacts.
// Type metadata is pre-resolved in the design layout (slot_type_infos).
// No mir::Design or TypeArena dependency.
auto BuildInspectionPlan(
    const Layout& layout, std::span<const InspectedVarRef> refs)
    -> InspectionPlan;

}  // namespace lyra::lowering::mir_to_llvm
