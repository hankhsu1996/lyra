#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/execution_mode.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

auto LowerEffectOp(
    Context& context, const mir::EffectOp& effect_op,
    const ActiveExecutionMode& mode) -> Result<void>;

auto LowerEffectOp(
    Context& context, SlotAccessResolver& resolver,
    const mir::EffectOp& effect_op, const ActiveExecutionMode& mode)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
