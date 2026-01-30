#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Returns true if the compute instruction is a math function (IEEE 1800 §20.8).
// This includes both real-typed ($ln, $sin, $pow, ...) and integral ($clog2).
auto IsMathCompute(Context& context, const mir::Compute& compute) -> bool;

// Evaluate a math function rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
// For 4-state $clog2 results, sets *unknown_out to the unknown plane.
auto LowerMathRvalue(
    Context& context, const mir::Compute& compute, llvm::Value** unknown_out)
    -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
