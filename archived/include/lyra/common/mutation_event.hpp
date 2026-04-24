#pragma once

#include <cstdint>
#include <functional>
#include <variant>

#include "lyra/common/range_set.hpp"

namespace lyra::common {

// Strong typedefs for mutation event roots.
struct DesignSlotId {
  uint32_t value = UINT32_MAX;
  auto operator==(const DesignSlotId&) const -> bool = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }
};

struct HeapObjId {
  uint32_t value = UINT32_MAX;
  auto operator==(const HeapObjId&) const -> bool = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }
};

enum class MutationKind : uint8_t {
  kValueWrite,           // Scalar/element value assignment
  kBulkInit,             // $readmemh, $fread memory variant
  kStructuralNoRealloc,  // Queue pop, element delete
  kStructuralRealloc,    // Queue push, insert, dynarray resize
};

// Explicit epoch contract -- not implied by Kind name.
enum class EpochEffect : uint8_t {
  kNone,        // No epoch impact (value writes, pops that don't realloc)
  kBump,        // Epoch increments (realloc, structural change)
  kInvalidate,  // Handle/backing invalidated (delete, free)
};

struct MutationEvent {
  std::variant<DesignSlotId, HeapObjId> root;
  MutationKind kind = MutationKind::kValueWrite;
  EpochEffect epoch_effect = EpochEffect::kNone;
  RangeSet ranges;  // Full-extent (default) or precise sub-ranges.
};

// Callable sink for mutation events.
using MutationSink = std::function<void(const MutationEvent&)>;

}  // namespace lyra::common
