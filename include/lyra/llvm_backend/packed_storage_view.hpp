#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit/signal_id_expr.hpp"
#include "lyra/mir/handle.hpp"

namespace llvm {
class Type;
}  // namespace llvm

namespace lyra::lowering::mir_to_llvm {

class Context;

// Canonical descriptor for one packed storage root.
//
// 2-state: one value plane (unk_plane_offset_bytes == 0).
// 4-state: value plane at offset 0, unknown plane at unk_plane_offset_bytes.
struct PackedStorageView {
  llvm::Value* base_ptr = nullptr;
  uint32_t total_semantic_bits = 0;
  uint32_t value_plane_bytes = 0;
  uint32_t unk_plane_offset_bytes = 0;
  bool is_four_state = false;
};

// A single step in a packed projection path.
//
// Currently only BitRangeProjection-backed steps are extracted from MIR.
// The data structure supports both dynamic offsets (runtime-computed, from
// packed array element access) and static offsets (compile-time constant).
// Current extraction only emits dynamic BitRangeProjection steps; static
// offsets are reserved for future packed struct field and part-select steps.
//
// The dynamic_bit_offset is an LLVM Value* because path extraction lowers
// MIR operands to LLVM IR for dynamic indices.
struct PackedProjectionStep {
  // Bit offset for this step. Exactly one should be set:
  // - dynamic_bit_offset != nullptr for runtime-computed offsets
  // - static_bit_offset used when dynamic_bit_offset == nullptr
  llvm::Value* dynamic_bit_offset = nullptr;
  uint32_t static_bit_offset = 0;

  uint32_t semantic_bits = 0;
  TypeId result_type;

  // True when the bit offset contributed by this step is provably a
  // multiple of 8 at all runtime values.
  //
  // Currently only meaningful for BitRangeProjection-backed steps, where
  // the proof is: offset = index * element_width, so if element_width is
  // a multiple of 8, the offset is always byte-aligned. See
  // IsBitRangeStepProvablyByteAligned(). Future step kinds (packed struct
  // fields, part-selects) will need their own alignment proof rules.
  bool is_provably_byte_aligned = false;
};

// Packed access path extracted from MIR place/projection walking.
//
// Currently scoped to BitRangeProjection-backed packed subviews only.
// Does not handle packed struct fields or nested packed aggregates.
//
// The storage view is resolved eagerly during extraction. This folds
// storage resolution into path extraction as a temporary simplification
// for the BitRangeProjection-only scope. Future generalized packed access
// may want to separate path extraction from storage resolution.
struct PackedAccessPath {
  PackedStorageView storage;
  std::vector<PackedProjectionStep> steps;
  TypeId result_type;
};

enum class PackedSubviewKind {
  kByteAddressable,
  kBitAddressable,
};

// Resolved localized-access descriptor for a packed subobject.
//
// Current model: represents the final localized subview with a single
// classification. Future mixed refinement (outer byte-addressable base +
// inner non-byte-aligned bit refinement) will require introducing an
// intermediate localized base plus local bit offset. That is not modeled
// here yet; the current shape collapses to one final offset and kind.
struct PackedSubviewAccess {
  PackedStorageView storage;
  PackedSubviewKind kind = PackedSubviewKind::kBitAddressable;

  // Always set: the composed bit offset and width.
  llvm::Value* semantic_bit_offset = nullptr;
  uint32_t semantic_bit_width = 0;

  // Only valid for kByteAddressable:
  llvm::Value* byte_offset = nullptr;
  uint32_t storage_byte_width = 0;

  TypeId result_type;
};

// Backend-owned SSA carrier for packed values.
struct PackedRValue {
  llvm::Value* val = nullptr;
  llvm::Value* unk = nullptr;
  uint32_t semantic_bits = 0;
  uint32_t storage_bytes = 0;
  bool is_four_state = false;
};

// Semantic store mode for packed writes.
enum class PackedStoreMode {
  // Init path: direct store, no compare, no dirty mark, no engine.
  // signal_id, engine_ptr, first_dirty_seen are unused.
  kDirectInit,
  // Simulation path: compare + conditional dirty mark.
  // signal_id and engine_ptr required (engine_ptr non-null at runtime).
  // first_dirty_seen may be non-null for inline fast path.
  kNotifySimulation,
  // Cross-context: compare + conditional dirty mark.
  // signal_id required. engine_ptr required as LLVM value but may be
  // null at runtime (guarded branch in generated IR).
  // first_dirty_seen unused (not available in cross-context scope).
  kNotifyCrossContext,
};

// Write-side policy for packed stores, decoupled from access resolution.
struct PackedStorePolicy {
  PackedStoreMode store_mode = PackedStoreMode::kDirectInit;

  // True when dirty-mark is deferred to loop-exit edge.
  // Only interpreted inside EmitStoreToPackedSubview /
  // EmitSchedulePackedSubviewWrite; not branched on outside the module.
  bool notification_deferred = false;

  std::optional<SignalIdExpr> signal_id;
  llvm::Value* engine_ptr = nullptr;
  llvm::Value* first_dirty_seen = nullptr;
};

// Extract a packed access path from a MIR place with BitRangeProjection.
// Resolves the storage view eagerly. The place must have at least one
// BitRangeProjection. Currently only extracts BitRangeProjection-backed
// steps; does not handle packed struct fields or part-selects.
auto ExtractPackedAccessPath(Context& ctx, mir::PlaceId place_id)
    -> Result<PackedAccessPath>;

// Resolve a packed access path into a localized subview descriptor.
// This is the single classification point: byte-addressable or bit-addressable.
auto ResolvePackedSubview(Context& ctx, const PackedAccessPath& path)
    -> Result<PackedSubviewAccess>;

// Load a packed subview. Handles both kByteAddressable and kBitAddressable.
auto EmitLoadFromPackedSubview(Context& ctx, const PackedSubviewAccess& access)
    -> Result<PackedRValue>;

// Localized subview immediate write.
auto EmitStoreToPackedSubview(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value,
    const PackedStorePolicy& policy) -> Result<void>;

// Localized subview deferred/NBA write.
auto EmitSchedulePackedSubviewWrite(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value,
    const PackedStorePolicy& policy) -> Result<void>;

// Whole-value materialization: load full packed value from canonical storage.
auto MaterializePackedValue(Context& ctx, const PackedStorageView& storage)
    -> Result<PackedRValue>;

// Whole-value store-back: store full packed value to canonical storage.
auto StorePackedValue(
    Context& ctx, const PackedStorageView& storage, const PackedRValue& value,
    const PackedStorePolicy& policy) -> Result<void>;

// Top-level packed load for BitRangeProjection places.
// Runs the full pipeline: extract path -> resolve subview -> emit load ->
// convert to legacy LLVM value form.
auto LoadPackedPlace(
    Context& ctx, mir::PlaceId place_id, llvm::Type* target_type)
    -> Result<llvm::Value*>;

// Convert a PackedRValue to legacy LLVM value form ({iN, iN} for 4-state,
// iN for 2-state). Used at the module boundary where callers still expect
// raw llvm::Value* in the old representation.
auto ConvertPackedRValueToLegacyLlvmValue(
    Context& ctx, const PackedRValue& rval, llvm::Type* target_type)
    -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
