#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

// Lower blocking assignment (immediate write).
auto LowerAssign(Context& context, const mir::Assign& assign) -> Result<void>;

// Resolver-aware: route module-slot reads/writes through the resolver.
auto LowerAssign(
    Context& context, SlotAccessResolver& resolver, const mir::Assign& assign)
    -> Result<void>;

// Lower guarded assignment (conditional immediate write).
auto LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded)
    -> Result<void>;

// Resolver-aware overload.
auto LowerGuardedAssign(
    Context& context, SlotAccessResolver& resolver,
    const mir::GuardedAssign& guarded) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
