#pragma once

#include <cstdint>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Load a typed value from a flat bit-blob at a given offset.
// Recursively handles unpacked structs, arrays, and unions.
// Returns error if floating-point type read encounters X/Z bits.
auto LoadFromBlob(
    TypeId type_id, const RuntimeIntegral& storage, uint32_t bit_offset,
    const TypeArena& types) -> Result<RuntimeValue>;

// Store a typed value into a flat bit-blob at a given offset.
// Recursively handles unpacked structs, arrays, and unions.
// 2-state canonicalization: for 2-state destination types, X/Z masks are
// force-cleared.
void StoreToBlob(
    TypeId type_id, const RuntimeValue& val, RuntimeIntegral& storage,
    uint32_t bit_offset, const TypeArena& types);

// Recursively check if a runtime value contains X/Z bits.
// Used for float-containing union write rejection.
auto ValueContainsXZ(
    const RuntimeValue& val, TypeId type_id, const TypeArena& types) -> bool;

}  // namespace lyra::mir::interp
