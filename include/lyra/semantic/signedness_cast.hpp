#pragma once

#include <expected>
#include <string>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::semantic {

// Returns TypeId with signedness set to signed, or error message.
// Input must be packed integral (kIntegral, kPackedArray, kPackedStruct).
// Preserves width, 2-state/4-state - only flips is_signed bit.
auto MakeSignedVariant(TypeId type, TypeArena& arena)
    -> std::expected<TypeId, std::string>;

// Returns TypeId with signedness set to unsigned, or error message.
// Input must be packed integral (kIntegral, kPackedArray, kPackedStruct).
// Preserves width, 2-state/4-state - only flips is_signed bit.
auto MakeUnsignedVariant(TypeId type, TypeArena& arena)
    -> std::expected<TypeId, std::string>;

// Returns canonical 2-state unsigned bit vector: bit[width-1:0]
auto GetBitVectorType(TypeArena& arena, uint32_t width) -> TypeId;

}  // namespace lyra::semantic
