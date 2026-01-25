#pragma once

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

// Verify MIR function invariants. Throws InternalError on failure.
//
// Invariants checked:
// - param_local_slots.size() == signature.params.size()
// - Each param slot < local_types.size()
// - Param slots are unique (no aliasing)
// - Non-void function: all Return terminators must have a value
// - Void function: all Return terminators must NOT have a value
void VerifyFunction(
    const Function& func, const Arena& arena, const TypeArena& types);

// Verify MIR process invariants. Throws InternalError on failure.
//
// Invariants checked:
// - All Return terminators must NOT have a value (processes don't return)
void VerifyProcess(
    const Process& proc, const Arena& arena, const TypeArena& types);

}  // namespace lyra::mir
