#pragma once

#include <cstdint>
#include <optional>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// WriteTarget bundles the information needed for a write operation.
// All fields are derived from the same canonicalized (alias-resolved) place,
// ensuring consistency between the pointer and the signal_id used for notify.
//
// Note: This struct is intentionally minimal. The byte_size for notification
// is derived from the value being stored (value->getType()), not stored here.
// This eliminates redundancy and the need for call-site mutation.
struct WriteTarget {
  llvm::Value* ptr = nullptr;  // Pointer to canonical storage
  std::optional<uint32_t>
      canonical_signal_id;  // Root slot ID, nullopt if not design
};

namespace commit {

// Accessor for commit-module-only Context methods.
// Provides controlled access to WriteTarget resolution and design-slot queries.
// Only commit module code should use this class.
class Access {
 public:
  // Get unified write target (pointer + signal_id) from a place.
  // All fields derived from the same alias-resolved place, ensuring
  // consistency.
  [[nodiscard]] static auto GetWriteTarget(Context& ctx, mir::PlaceId target)
      -> Result<WriteTarget>;

  // Get the canonical root signal_id for a place (after alias resolution).
  // Returns nullopt if the resolved root is not a design slot.
  [[nodiscard]] static auto GetCanonicalRootSignalId(
      Context& ctx, mir::PlaceId target) -> std::optional<uint32_t>;

  // Check if a place targets a design slot (has signal_id after alias
  // resolution).
  [[nodiscard]] static auto IsDesignSlot(Context& ctx, mir::PlaceId target)
      -> bool;
};

}  // namespace commit
}  // namespace lyra::lowering::mir_to_llvm
