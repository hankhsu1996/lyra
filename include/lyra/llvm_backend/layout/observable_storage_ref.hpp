#pragma once

#include <variant>

#include "lyra/llvm_backend/layout/storage_types.hpp"

namespace lyra::lowering::mir_to_llvm {

// Observable storage reference: body-relative offset.
// Constructor adds instance_byte_base to produce the realized absolute offset.
struct ObservableBodyRelativeStorageRef {
  BodyByteOffset offset;
};

// Observable storage reference: owner-absolute offset.
// Used for forwarded alias slots whose storage lives in a different instance.
// Constructor uses the offset as-is (already arena-absolute).
struct ObservableOwnerAbsoluteStorageRef {
  ArenaByteOffset offset;
};

// Semantic observable storage reference for lowering.
// Callers must pattern-match the variant to access the offset, ensuring
// body-relative and owner-absolute offsets cannot be confused.
// Translated to the packed runtime ABI format (flags + uint32_t) only at
// the final emission boundary.
using ObservableStorageRef = std::variant<
    ObservableBodyRelativeStorageRef, ObservableOwnerAbsoluteStorageRef>;

}  // namespace lyra::lowering::mir_to_llvm
