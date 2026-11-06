#pragma once

#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/clocking_block_id.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"

namespace lyra::lowering::hir_to_mir {

// What a clocking block does at each occurrence of its clocking event (LRM
// 14.7: one copy per instance of its scope, like an always procedure): count
// the occurrence, which a drive matures by (LRM 14.16), and then, in the
// Observed region of that time step, sample each input and trigger the block's
// event, so every process waiting on the block reads what it sampled (LRM
// 14.10, 14.13). It runs inside each occurrence, so a drive made by anything
// the occurrence wakes counts the occurrence it made.
[[nodiscard]] auto LowerClockingSampler(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    hir::ClockingBlockId id) -> diag::Result<mir::CallableDecl>;

// What keeps the copy an input skewed by a delay reads (LRM 14.13): each value
// its signal takes lands in the copy that delay later, in the NBA region of
// that slot, so at a clocking event the copy holds what the signal held at the
// end of the time step that long before, ahead of the Observed region reading
// it, whichever region set the scope's code runs in.
[[nodiscard]] auto LowerClockingFollower(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    hir::ClockingBlockId id, hir::ClockvarId clockvar)
    -> diag::Result<mir::CallableDecl>;

// What lands a drive of the output `landing` names (LRM 14.16), a task of the
// scope taking the drive's count of cycles, in the role `virtual_dispatch`
// names: it waits for the occurrence of the clocking event the drive matures
// at -- counted from the occurrence of the drive's time step where that has
// happened when the drive is made, and from the next otherwise -- then for the
// output's skew from there, and returns in the Re-NBA region of the slot that
// names (LRM 14.4), where the drive writes the output's driving variable. An
// output onto a variable returns once that pass's end lets its drives write,
// so the variable takes only the last value of the pass (LRM 14.16.2). A
// drive running it is woken inside each occurrence it waits for, so the skew
// is counted from the occurrence itself.
[[nodiscard]] auto LowerClockingLanding(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    hir::ClockingOutputLanding landing,
    std::optional<mir::VirtualDispatchRole> virtual_dispatch)
    -> diag::Result<mir::CallableDecl>;

}  // namespace lyra::lowering::hir_to_mir
