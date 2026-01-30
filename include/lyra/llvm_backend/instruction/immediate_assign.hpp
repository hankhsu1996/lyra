#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower blocking assignment (immediate write).
// Handles both Operand and Rvalue sources via RightHandSide dispatch.
// For bit-range projections, uses read-modify-write (load, splice, store).
// For full writes, delegates to TypeOps for semantic assignment.
auto LowerAssign(Context& context, const mir::Assign& assign) -> Result<void>;

// Lower guarded assignment (conditional immediate write).
// Semantics: if (guard) dest := rhs
// rhs is evaluated BEFORE the guard check (per SystemVerilog spec).
// Internally wraps the same immediate-write logic with a conditional branch.
auto LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
