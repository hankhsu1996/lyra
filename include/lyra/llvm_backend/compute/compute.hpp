#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerCompute(Context& context, const mir::Compute& compute)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
