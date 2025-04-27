#pragma once

#include "lowering/mir_to_lir/module.hpp"

namespace lyra::lowering {

// Main entry point for MIR to LIR lowering.
// See lowering/mir_to_lir/module.hpp for implementation.
auto MirToLir(const mir::Module& module) -> lir::Module;

}  // namespace lyra::lowering
