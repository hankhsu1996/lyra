#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower regular assignment (blocking).
// Delegates to TypeOps for semantic assignment, or uses read-modify-write for
// bit-range projections.
auto LowerAssign(Context& context, const mir::Assign& assign) -> Result<void>;

// Lower guarded assignment (conditional write based on validity check).
// Source is evaluated BEFORE the validity check (per SystemVerilog spec).
auto LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded)
    -> Result<void>;

// Lower non-blocking assignment (NBA).
// Schedules the write for the NBA region and handles notification.
// Supports bit-range, index projection, and simple full-width writes.
auto LowerNonBlockingAssign(Context& context, const mir::NonBlockingAssign& nba)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
