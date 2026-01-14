#pragma once

#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::lowering::mir_to_lir {

// Lowers a MIR Process into a LIR Process.
auto LowerProcess(
    const mir::Process& process, LirBuilder& builder,
    LoweringContext& lowering_context) -> void;

}  // namespace lyra::lowering::mir_to_lir
