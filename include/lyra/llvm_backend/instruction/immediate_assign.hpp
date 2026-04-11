#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

auto LowerAssign(
    Context& context, const CuFacts& facts, const mir::Assign& assign)
    -> Result<void>;

auto LowerAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Assign& assign) -> Result<void>;

auto LowerGuardedAssign(
    Context& context, const CuFacts& facts, const mir::GuardedAssign& guarded)
    -> Result<void>;

auto LowerGuardedAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::GuardedAssign& guarded) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
