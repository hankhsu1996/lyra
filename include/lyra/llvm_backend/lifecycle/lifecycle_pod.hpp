#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

namespace detail {

// Copy POD type: load from src, store to dst.
// For non-managed types only (scalars, packed arrays, etc.).
void CopyInitPod(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);

}  // namespace detail
}  // namespace lyra::lowering::mir_to_llvm
