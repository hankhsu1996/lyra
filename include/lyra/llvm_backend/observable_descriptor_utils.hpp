// Backend canonical observable-shape model for runtime descriptor packing.
//
// This file defines the backend's canonical observable-shape model used
// before runtime descriptor packing. Canonical observable shape comes
// from the storage owner:
//   - Alias-group-invariant trace/storage fields use the owner slot.
//   - Per-slot identity fields (name, provenance, flags) stay caller-owned.
// This model is owner-based only: alias-slot-local coordinates are not inputs.
// Classification helpers are pure and do not depend on emission machinery.

#pragma once

#include <cstdint>
#include <optional>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"

namespace lyra::mir {
enum class SlotKind : uint8_t;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

struct DesignLayout;
struct RealizationData;

// Typed wrapper for an unvalidated observable owner slot coordinate.
// Use ComputeCanonicalObservableShape() to validate canonical-ness.
struct ObservableOwnerSlotId {
  [[nodiscard]] static auto Create(uint32_t raw) -> ObservableOwnerSlotId {
    return ObservableOwnerSlotId{raw};
  }
  [[nodiscard]] auto Raw() const -> uint32_t {
    return value_;
  }

 private:
  uint32_t value_ = 0;
  explicit ObservableOwnerSlotId(uint32_t v) : value_(v) {
  }
};

// 4-state lane layout within a packed storage spec.
struct Packed4LaneShape {
  uint32_t value_lane_byte_offset = 0;
  uint32_t value_lane_byte_size = 0;
  uint32_t unk_lane_byte_offset = 0;
  uint32_t unk_lane_byte_size = 0;
};

// Alias-group-invariant trace shape. Always derived from the canonical
// storage owner slot, never from the alias slot itself.
struct CanonicalTraceShape {
  uint32_t bit_width = 0;
  runtime::TraceSignalKind trace_kind = runtime::TraceSignalKind::kVariable;
};

// Alias-group-invariant storage shape. Derived from the canonical owner's
// layout storage spec. Lane fields are present only for kPacked4.
struct CanonicalStorageShape {
  runtime::SlotStorageKind storage_kind = runtime::SlotStorageKind::kPacked2;
  uint32_t total_bytes = 0;
  std::optional<Packed4LaneShape> packed4_lanes;
};

// Combined owner-canonical observable shape. All fields derived from the
// canonical storage owner. Per-slot identity fields (name, provenance,
// storage reference relocation, flags) are NOT part of this struct.
struct CanonicalObservableShape {
  CanonicalTraceShape trace;
  CanonicalStorageShape storage;
};

// Derive runtime slot storage kind from resolved storage spec.
auto ClassifySlotStorageKind(const SlotStorageSpec& spec)
    -> runtime::SlotStorageKind;

// Map MIR SlotKind to runtime TraceSignalKind.
auto MapSlotKindToTraceKind(mir::SlotKind kind) -> runtime::TraceSignalKind;

// Compute trace bit width from a type. Returns the packed bit width for
// integral/packed types, 0 for non-bit-vector types.
auto ComputeTraceBitWidth(TypeId type_id, const TypeArena& types) -> uint32_t;

// Compute alias-group-invariant observable shape from the canonical owner.
// The result is derived from owner-canonical layout/type/kind data and is
// independent of any alias-slot-local declaration kind or storage row.
// Throws InternalError if owner is out of range or not self-owning.
auto ComputeCanonicalObservableShape(
    ObservableOwnerSlotId owner, const DesignLayout& layout,
    const RealizationData& realization, const TypeArena& type_arena)
    -> CanonicalObservableShape;

}  // namespace lyra::lowering::mir_to_llvm
