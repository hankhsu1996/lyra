#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

// Compute a slot-normalized fingerprint for a MIR process.
// Two processes from different instances of the same module definition
// are shareable if and only if their fingerprints match AND the layout
// byte-offset compatibility check passes.
//
// The hash normalizes design slot references by subtracting base_slot_id,
// making it invariant across instances with different absolute slot IDs.
// FunctionId references are normalized to their index in module_functions.
// TypeIds are resolved to type shapes (not pointer identity) for
// cross-instance stability.
//
// Determinism: traversal order is fixed (block index, statement index,
// operand index). No unordered_map iteration in the hash path.
auto ComputeProcessFingerprint(
    const mir::Process& process, const mir::Arena& arena,
    const TypeArena& types, uint32_t base_slot_id,
    const std::vector<mir::FunctionId>& module_functions) -> uint64_t;

}  // namespace lyra::lowering::mir_to_llvm
