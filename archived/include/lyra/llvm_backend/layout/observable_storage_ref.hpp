#pragma once

#include "lyra/llvm_backend/layout/storage_types.hpp"

namespace lyra::lowering::mir_to_llvm {

// Observable storage reference: body-relative offset.
// Constructor adds instance_byte_base to produce the realized absolute offset.
// After R2, all body-local observable slots use this type.
struct ObservableBodyRelativeStorageRef {
  BodyByteOffset offset;
};

}  // namespace lyra::lowering::mir_to_llvm
