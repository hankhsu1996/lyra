#pragma once

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Unified cast lowering for CastRvalueInfo.
// Handles all cast combinations:
//   - 2s-int <-> 2s-int
//   - 2s-int <-> 4s-int
//   - 4s-int <-> 4s-int
//   - int <-> float
// The target type determines the storage format (2-state vs 4-state).
void LowerCastUnified(Context& context, const mir::Compute& compute);

// Unified bitcast lowering for BitCastRvalueInfo.
// Handles reinterpretation casts between integral and floating-point types.
void LowerBitCastUnified(Context& context, const mir::Compute& compute);

}  // namespace lyra::lowering::mir_to_llvm
