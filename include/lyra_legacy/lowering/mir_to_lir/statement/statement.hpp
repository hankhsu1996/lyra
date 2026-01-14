#pragma once

#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_lir {

// Lowers a MIR Statement into LIR instructions.
auto LowerStatement(
    const mir::Statement& statement, LirBuilder& builder,
    LoweringContext& lowering_context) -> void;

}  // namespace lyra::lowering::mir_to_lir
