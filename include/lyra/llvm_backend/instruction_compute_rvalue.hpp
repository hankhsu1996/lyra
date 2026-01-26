#pragma once

#include <cstdint>

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

enum class PlaceKind {
  kIntegral,
  kString,
};

struct PlaceTypeInfo {
  PlaceKind kind;
  uint32_t bit_width;
  bool is_four_state;
};

// Validate target place type and return info for dispatch.
// Throws UnsupportedErrorException for non-packed, non-string types.
auto ValidateAndGetTypeInfo(Context& context, mir::PlaceId place_id)
    -> PlaceTypeInfo;

}  // namespace lyra::lowering::mir_to_llvm
