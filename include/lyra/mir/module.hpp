#pragma once

#include "lyra/common/symbol_types.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// Instance-side MIR record.
//
// This represents one elaborated module instance in the design and references
// its behavioral MIR through body_id. It does not own behavioral IR.
struct Module {
  SymbolId instance_sym;
  ModuleBodyId body_id;
};

}  // namespace lyra::mir
