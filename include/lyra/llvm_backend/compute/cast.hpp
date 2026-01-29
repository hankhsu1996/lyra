#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
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
auto LowerCastUnified(Context& context, const mir::Compute& compute)
    -> Result<void>;

// Unified bitcast lowering for BitCastRvalueInfo.
// Handles reinterpretation casts between integral and floating-point types.
auto LowerBitCastUnified(Context& context, const mir::Compute& compute)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
