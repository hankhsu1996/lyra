#pragma once

#include "lyra/common/symbol_types.hpp"

namespace lyra::mir {

struct ModuleBody;

// Instance-side MIR record.
//
// This represents one elaborated module instance in the design and points
// directly at its behavioral MIR. It does not own behavioral IR.
// The body pointer is stable: mir::Design::module_bodies is finalized
// before mir::Module records are created and is never resized afterward.
struct Module {
  SymbolId instance_sym;
  const ModuleBody* body = nullptr;
};

}  // namespace lyra::mir
