#pragma once

#include "lyra/lowering/mir_to_lir/module.hpp"

namespace lyra::common {
class SymbolTable;
}

namespace lyra::lowering::mir_to_lir {

// Main entry point for MIR to LIR lowering.
// See lowering/mir_to_lir/module.hpp for implementation.
// symbol_table enables constant lookup for parameters during lowering.
// The reference is only used during the lowering call - LIR stores only
// SymbolId, not table pointers.
auto MirToLir(
    const mir::Module& module, const common::SymbolTable& symbol_table)
    -> std::unique_ptr<lir::Module>;

}  // namespace lyra::lowering::mir_to_lir
