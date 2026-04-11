#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/execution_mode.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

auto LowerEffectOp(
    Context& context, const CuFacts& facts, const mir::EffectOp& effect_op,
    const ActiveExecutionMode& mode, const BodySiteContext& site_ctx)
    -> Result<void>;

auto LowerEffectOp(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::EffectOp& effect_op, const ActiveExecutionMode& mode,
    const BodySiteContext& site_ctx) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
