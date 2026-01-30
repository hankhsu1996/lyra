#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower regular assignment (blocking).
// Handles both Operand and Rvalue sources via RightHandSide dispatch.
// Delegates to TypeOps for semantic assignment, or uses read-modify-write for
// bit-range projections.
auto LowerAssign(Context& context, const mir::Assign& assign) -> Result<void>;

// Lower guarded store (conditional write based on validity check).
// Source is evaluated BEFORE the validity check (per SystemVerilog spec).
// Handles both Operand and Rvalue sources via RightHandSide dispatch.
auto LowerGuardedStore(Context& context, const mir::GuardedStore& guarded)
    -> Result<void>;

// Lower non-blocking assignment (NBA).
// Schedules the write for the NBA region and handles notification.
// Supports bit-range, index projection, and simple full-width writes.
auto LowerNonBlockingAssign(Context& context, const mir::NonBlockingAssign& nba)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
