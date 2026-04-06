#pragma once

#include <cstdint>

#include "lyra/semantic/decision.hpp"

namespace lyra::lowering::hir_to_mir {

// Body-level decision site ID allocator.
// Owns the monotonic counter for body-global DecisionId assignment.
// All MirBuilders within the same module body share one allocator so
// decision IDs are unique across processes and functions in the body.
//
// Package/design-global paths that do not need body-global uniqueness
// may use a separate allocator instance per callable.
class DecisionSiteAllocator {
 public:
  auto Allocate() -> semantic::DecisionId {
    return semantic::DecisionId::FromIndex(next_id_++);
  }

  [[nodiscard]] auto TotalAllocated() const -> uint32_t {
    return next_id_;
  }

 private:
  uint32_t next_id_ = 0;
};

}  // namespace lyra::lowering::hir_to_mir
