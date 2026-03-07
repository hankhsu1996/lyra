#pragma once

#include <vector>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

// Compute a fingerprint for a MIR process.
// Two processes from different instances of the same module definition
// are shareable if and only if their fingerprints match AND the layout
// byte-offset compatibility check passes.
//
// kModuleSlot roots hash body-local IDs directly (already 0-based).
// kDesignGlobal roots hash global IDs directly.
// FunctionId references are normalized to their index in module_functions.
// TypeIds are resolved to type shapes (not pointer identity) for
// cross-instance stability.
//
// Determinism: traversal order is fixed (block index, statement index,
// operand index). No unordered_map iteration in the hash path.
auto ComputeProcessFingerprint(
    const mir::Process& process, const mir::Arena& arena,
    const TypeArena& types,
    const std::vector<mir::FunctionId>& module_functions) -> uint64_t;

}  // namespace lyra::lowering::mir_to_llvm
