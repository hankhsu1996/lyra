#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Cast lowering - returns computed value and sets *unknown_out for 4-state.
auto LowerCastRvalue(
    Context& context, const mir::Compute& compute, llvm::Value** unknown_out)
    -> Result<llvm::Value*>;

// Bitcast lowering - returns computed value and sets *unknown_out for 4-state.
auto LowerBitCastRvalue(
    Context& context, const mir::Compute& compute, llvm::Value** unknown_out)
    -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
