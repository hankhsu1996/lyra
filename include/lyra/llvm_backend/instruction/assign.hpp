#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower regular assignment (blocking).
// Handles both Operand and Rvalue sources via RightHandSide dispatch.
// Delegates to TypeOps for semantic assignment, or uses read-modify-write for
// bit-range projections.
auto LowerAssign(Context& context, const mir::Assign& assign) -> Result<void>;

// Lower guarded assignment (conditional write based on guard).
// rhs is evaluated BEFORE the guard check (per SystemVerilog spec).
// Handles both Operand and Rvalue sources via RightHandSide dispatch.
auto LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded)
    -> Result<void>;

// Lower deferred assignment (NBA region).
// Schedules the write for the NBA region and handles notification.
// Supports bit-range, index projection, and simple full-width writes.
auto LowerDeferredAssign(Context& context, const mir::DeferredAssign& deferred)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
