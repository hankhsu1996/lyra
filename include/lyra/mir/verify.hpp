#pragma once

#include <string_view>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

// Verify MIR function invariants. Throws InternalError on failure.
// label: descriptive name for error messages (e.g., "top.u_alu: func foo").
//
// Invariants checked:
// - param_local_slots.size() == signature.params.size()
// - Each param slot < local_types.size()
// - Param slots are unique (no aliasing)
// - Non-void function: all Return terminators must have a value
// - Void function: all Return terminators must NOT have a value
// - BlockParam temp_ids are unique across all blocks
// - For Jump/Branch: edge arg count == target block param count
// - For Jump/Branch: edge arg types match target block param types
// - All UseTemp operands in edge args reference defined temp_ids
void VerifyFunction(
    const Function& func, const Arena& arena, const TypeArena& types,
    std::string_view label = "function");

// Verify MIR process invariants. Throws InternalError on failure.
// label: descriptive name for error messages (e.g., "top.u_alu: always[3]").
//
// Invariants checked:
// - All Return terminators must NOT have a value (processes don't return)
// - BlockParam temp_ids are unique across all blocks
// - For Jump/Branch: edge arg count == target block param count
// - For Jump/Branch: edge arg types match target block param types
// - All UseTemp operands in edge args reference defined temp_ids
void VerifyProcess(
    const Process& proc, const Arena& arena, const TypeArena& types,
    std::string_view label = "process");

}  // namespace lyra::mir
