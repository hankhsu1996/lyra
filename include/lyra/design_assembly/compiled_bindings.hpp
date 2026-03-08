#pragma once

#include <vector>

#include "lyra/common/symbol_types.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/port_connection.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::design_assembly {

struct CompiledConnectionBody {
  mir::Process process;
};

struct CompiledDriveBinding {
  mir::PortConnection::Kind kind =
      mir::PortConnection::Kind::kDriveParentToChild;
  SymbolId child_port_sym;
  SymbolId parent_instance_sym;

  mir::PlaceId child_place;
  mir::PlaceId parent_place;

  CompiledConnectionBody body;
};

struct CompiledAliasBinding {
  SymbolId child_port_sym;
  SymbolId parent_instance_sym;

  mir::PlaceId child_place;
  mir::PlaceId parent_place;
  mir::SlotId child_slot;
};

struct CompiledBindingPlan {
  std::vector<CompiledDriveBinding> drive_bindings;
  std::vector<CompiledAliasBinding> alias_bindings;
};

}  // namespace lyra::design_assembly
