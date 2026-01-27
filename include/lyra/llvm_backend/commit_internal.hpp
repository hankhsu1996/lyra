#pragma once

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

// Check if a place targets a design slot (has signal_id after alias
// resolution). Commit-internal only: external callers should not query
// design-slot status. Use case: feature gating for unsupported design-slot
// operations (e.g., struct with managed fields).
inline auto IsDesignSlotInternal(Context& ctx, mir::PlaceId target) -> bool {
  return ctx.GetCanonicalRootSignalId(target).has_value();
}

}  // namespace lyra::lowering::mir_to_llvm::detail
