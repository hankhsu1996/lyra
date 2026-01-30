#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Check if compute is a real-typed math operation (unary or binary).
// Does NOT match casts — casts are handled by LowerCastUnified.
auto IsRealMathCompute(Context& context, const mir::Compute& compute) -> bool;

// Evaluate a real-typed math rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
auto LowerRealRvalue(Context& context, const mir::Compute& compute)
    -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
