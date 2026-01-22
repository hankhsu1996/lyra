#pragma once

#include <optional>
#include <string>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::semantic {

// Validates that a BitCast from src to dst is legal.
// Returns fully formatted error message if invalid, nullopt if valid.
//
// Rules:
// 1. Exactly one side is real/shortreal, other is kIntegral (true integral)
// 2. Integral side: 2-state (!is_four_state), exact width (64 for real, 32 for
// shortreal)
// 3. Signedness on integral side: allowed (ignored)
//
// This is the single source of truth for BitCast legality - used by:
// - AST→HIR: to emit diagnostics at lowering time
// - MIR interpreter: to assert invariants in debug builds
// - LLVM backend: to assert invariants in debug builds
auto ValidateBitCast(const Type& src, const Type& dst, const TypeArena& arena)
    -> std::optional<std::string>;

}  // namespace lyra::semantic
