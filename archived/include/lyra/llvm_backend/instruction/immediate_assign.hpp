#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

// PlainAssign: pure trivial write, no lifecycle dispatch.
// Destination must not be a managed type.
auto LowerPlainAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::WriteTarget& dest, const mir::RightHandSide& rhs)
    -> Result<void>;

auto LowerPlainAssign(
    Context& context, const CuFacts& facts, const mir::WriteTarget& dest,
    const mir::RightHandSide& rhs) -> Result<void>;

// CopyAssign: explicit clone/copy lifecycle (destroy old, clone new).
auto LowerCopyAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::WriteTarget& dest, const mir::RightHandSide& rhs)
    -> Result<void>;

auto LowerCopyAssign(
    Context& context, const CuFacts& facts, const mir::WriteTarget& dest,
    const mir::RightHandSide& rhs) -> Result<void>;

// MoveAssign: explicit move lifecycle (destroy old, move new, cleanup source).
auto LowerMoveAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::WriteTarget& dest, const mir::RightHandSide& rhs)
    -> Result<void>;

auto LowerMoveAssign(
    Context& context, const CuFacts& facts, const mir::WriteTarget& dest,
    const mir::RightHandSide& rhs) -> Result<void>;

auto LowerGuardedAssign(
    Context& context, const CuFacts& facts, const mir::GuardedAssign& guarded)
    -> Result<void>;

auto LowerGuardedAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::GuardedAssign& guarded) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
