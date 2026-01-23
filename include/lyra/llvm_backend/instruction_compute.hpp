#pragma once

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

void LowerCompute(Context& context, const mir::Compute& compute);

}  // namespace lyra::lowering::mir_to_llvm
