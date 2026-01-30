#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower Compute instruction: evaluate rvalue and store to target.
// Calls compute::LowerRvalue to get the value, then handles storage.
auto LowerComputeInstruction(Context& context, const mir::Compute& compute)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
