#pragma once

#include <cstdint>

#include "lyra/common/symbol_types.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// Port connection record with asymmetric representation for different kinds.
// Source of truth for all port connections - alias_map is derived from this.
struct PortConnection {
  enum class Kind : int32_t {
    kDriveParentToChild,  // input: parent expr -> child port (via process)
    kDriveChildToParent,  // output variable: child port -> parent place
    kAlias,               // output/inout net: true identity
  };

  Kind kind = Kind::kDriveParentToChild;
  SymbolId child_port_sym;       // Child's port backing variable
  SymbolId parent_instance_sym;  // Parent module's instance symbol

  // For kDriveChildToParent and kAlias: both places are meaningful.
  // For kDriveParentToChild: only child_place is meaningful here;
  //   the source is the connection process itself (not a simple place).
  PlaceId child_place;   // Always valid
  PlaceId parent_place;  // Valid for kDriveChildToParent, kAlias
};

}  // namespace lyra::mir
