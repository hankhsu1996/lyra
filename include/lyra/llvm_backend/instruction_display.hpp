#pragma once

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

void LowerDisplayEffect(Context& context, const mir::DisplayEffect& display);

}  // namespace lyra::lowering::mir_to_llvm
