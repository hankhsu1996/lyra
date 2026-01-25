#pragma once

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Returns true if the compute instruction is a math function (IEEE 1800 §20.8).
// This includes both real-typed ($ln, $sin, $pow, ...) and integral ($clog2).
auto IsMathCompute(Context& context, const mir::Compute& compute) -> bool;

// Lowers a math function compute instruction.
// Internally dispatches based on operand type (real vs integral).
void LowerMathCompute(Context& context, const mir::Compute& compute);

}  // namespace lyra::lowering::mir_to_llvm
