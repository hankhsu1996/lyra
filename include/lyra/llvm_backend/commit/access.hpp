#pragma once

#include <cstdint>
#include <optional>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/commit/signal_id_expr.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/terminator.hpp"

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
  std::optional<SignalCoordExpr>
      canonical_signal_id;  // Root slot ID, nullopt if not design
  uint32_t dirty_off = 0;   // Byte offset within slot (write-side dirty range)
  uint32_t dirty_size = 0;  // 0 = full slot; > 0 = precise byte range
  // Canonical mutation-target signal identity. Resolved once from the
  // place root. Used for runtime observer queries at emission sites.
  // nullopt if the root has no mutation-target signal identity.
  std::optional<mir::SignalRef> mutation_signal;
  // Static dirty-propagation contract result: true iff behavioral,
  // connection, or design-global behavioral contracts require propagation.
  // Does NOT cover runtime observers (trace, rebind). Emission sites
  // must also check runtime observer state before suppressing.
  // Conservative default: true.
  bool requires_static_dirty_propagation = true;
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

  // Get the mutation-target signal_id for a place's root.
  // Resolves forwarded aliases to the storage owner for dirty-mark identity.
  // Returns nullopt if the root has no notifiable mutation-target signal
  // identity (e.g., local/temp roots that are not design storage).
  [[nodiscard]] static auto GetMutationTargetSignalCoord(
      Context& ctx, mir::PlaceId target) -> std::optional<SignalCoordExpr>;

  // Check if a place has a notifiable mutation-target signal identity.
  [[nodiscard]] static auto IsNotifiableMutationTarget(
      Context& ctx, mir::PlaceId target) -> bool;
};

}  // namespace commit
}  // namespace lyra::lowering::mir_to_llvm
