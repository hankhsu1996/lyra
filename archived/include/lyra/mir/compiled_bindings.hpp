#pragma once

#include <vector>

#include "lyra/common/symbol_types.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/port_connection.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

struct CompiledConnectionBody {
  Process process;
};

struct CompiledDriveBinding {
  PortConnection::Kind kind = PortConnection::Kind::kDriveParentToChild;
  SymbolId child_port_sym;
  SymbolId parent_instance_sym;

  PlaceId child_place;
  PlaceId parent_place;

  CompiledConnectionBody body;
};

struct CompiledBindingPlan {
  std::vector<CompiledDriveBinding> drive_bindings;
};

}  // namespace lyra::mir
