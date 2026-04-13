#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

auto LowerDisplayEffect(
    Context& context, const CuFacts& facts, const mir::DisplayEffect& display)
    -> Result<void>;

// Resolver-aware overload.
auto LowerDisplayEffect(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::DisplayEffect& display) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
