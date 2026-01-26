#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Check if compute is a real-typed math operation (unary or binary).
// Does NOT match casts — casts are handled by LowerCastUnified.
auto IsRealMathCompute(Context& context, const mir::Compute& compute) -> bool;
auto LowerRealCompute(Context& context, const mir::Compute& compute)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
