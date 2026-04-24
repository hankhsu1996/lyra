#pragma once

namespace lyra::lowering::mir_to_llvm {

// Ownership policy for assignment - determined once at entry, passed through
enum class OwnershipPolicy { kMove, kClone };

}  // namespace lyra::lowering::mir_to_llvm
