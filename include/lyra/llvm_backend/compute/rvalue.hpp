#pragma once

#include <cstdint>

#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
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
// Returns error for non-packed, non-string types.
auto ValidateAndGetTypeInfo(Context& context, mir::PlaceId place_id)
    -> Result<PlaceTypeInfo>;

// Context for lowering packed (integral) compute operations.
// Contains all type information needed by both 2-state and 4-state lowering.
struct PackedComputeContext {
  llvm::Type* storage_type;  // The LLVM type for the target place
  llvm::Type* element_type;  // For operations: iN for both 2s and 4s
  uint32_t bit_width;        // Semantic width for masking/shifts
  bool is_four_state;        // Drives the single branch point
};

}  // namespace lyra::lowering::mir_to_llvm
