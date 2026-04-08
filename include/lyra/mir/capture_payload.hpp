#pragma once

#include <vector>

#include "lyra/common/type.hpp"

namespace lyra::mir {

// Typed payload descriptor for by-value capture in deferred assertion
// actions. Used by the realization layer (DeferredUserCallRealization)
// and LLVM backend to build the payload struct type.
struct CapturePayloadDesc {
  // By-value captured types, ordered by call-order position (skipping
  // ref actuals). The LLVM backend derives the storage type from each
  // TypeId via ClassifyCallableValueAbi at emission time.
  std::vector<TypeId> field_types;
};

}  // namespace lyra::mir
