#pragma once

#include "lyra/lowering/mir_to_llvm/context.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

void LowerDisplayEffect(Context& context, const mir::DisplayEffect& display);

}  // namespace lyra::lowering::mir_to_llvm
