#pragma once

#include "lyra/lowering/mir_to_lir/module.hpp"

namespace lyra::lowering {

// Main entry point for MIR to LIR lowering.
// See lowering/mir_to_lir/module.hpp for implementation.
auto MirToLir(const mir::Module& module) -> std::unique_ptr<lir::Module>;

}  // namespace lyra::lowering
