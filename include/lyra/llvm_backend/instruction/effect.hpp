#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower effect operation (display, strobe, monitor, time format, memory I/O).
// Dispatches to specific effect handlers based on the variant type.
auto LowerEffectOp(Context& context, const mir::EffectOp& effect_op)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
