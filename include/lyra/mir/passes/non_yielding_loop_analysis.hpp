#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::passes {

// A canonical induction loop that qualifies for deferred notification.
//
// Within a qualifying loop, design-slot writes commit their values
// immediately to DesignState but defer the dirty-mark notification to
// the loop-exit edge. This is safe because no scheduler-visible
// observation can occur within a non-yielding loop body.
//
// Stage 1 scope: canonical increasing for-loops only (reuses existing
// canonical induction-loop recognition). All blocks must pass the
// positive whitelist for terminators, statements, and store paths.
//
// notified_roots contains the canonical notification unit for each
// design slot written in the loop, deduplicated by root identity.
// Multiple projected writes to the same root (e.g., data[0], data[i])
// collapse to one entry.
//
// region_blocks contains only the loop interior (header, body, latch).
// exit_block is the successor that receives the loop-exit edge effect
// and is guaranteed NOT to appear in region_blocks. This invariant
// ensures codegen never both suppresses notifications and emits
// deferred marks for the same block.
struct DeferredNotificationLoop {
  uint32_t header;
  uint32_t latch;
  // The header's Branch false-target. Receives deferred dirty-marks
  // as a loop-exit edge effect. Never included in region_blocks.
  uint32_t exit_block;
  // Blocks where notification policy is kDeferred (header + body + latch).
  std::vector<uint32_t> region_blocks;
  std::vector<SignalRef> notified_roots;
};

// Find canonical induction loops that qualify for deferred notification.
//
// A loop qualifies when every block in the loop (header, body, latch)
// satisfies the stage-1 positive semantic subset:
//   - terminators: Jump, Branch, Switch only
//   - statements: DefineTemp, Assign, GuardedAssign only
//   - effects: DisplayEffect, SeverityEffect, MonitorControlEffect,
//     TimeFormatEffect only
//   - design-slot writes: root type must use a packed/scalar commit
//     path (not container, string, or managed type)
//
// Nested loops: outermost qualifying loop subsumes inner loops.
// Inner loops may still qualify independently if the outer does not.
//
// Returns one entry per qualifying loop. Non-qualifying loops are
// silently skipped.
auto FindDeferredNotificationLoops(
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const TypeArena& types) -> std::vector<DeferredNotificationLoop>;

}  // namespace lyra::mir::passes
