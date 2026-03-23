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

// Descriptor for one packed storage root.
//
// 2-state: one value plane (unk_plane_offset_bytes == 0).
// 4-state: value plane at offset 0, unknown plane at unk_plane_offset_bytes.
//
// Storage kind:
// - canonical (design slots): byte-addressed arena storage where plane offsets
//   follow the canonical packed ABI (GetStorageByteSize /
//   FourStateUnknownLaneOffset).
// - non-canonical (process locals): LLVM-managed alloca storage where plane
//   layout follows LLVM struct alignment rules. Byte-addressable localized
//   access is not valid for non-canonical storage.
struct PackedStorageView {
  llvm::Value* base_ptr = nullptr;
  uint32_t total_semantic_bits = 0;
  uint32_t storage_plane_byte_size = 0;
  uint32_t unk_plane_offset_bytes = 0;
  bool is_four_state = false;

  // True for design slots (kModuleSlot, kDesignGlobal) where storage follows
  // the canonical byte-addressed packed ABI. False for process-local allocas
  // (kLocal, kTemp) where LLVM struct alignment may diverge from canonical.
  bool is_canonical_storage = false;

  // Non-null only when !is_canonical_storage. The LLVM type of the base
  // alloca, needed for typed load/store on the bit-addressable fallback path.
  llvm::Type* local_llvm_type = nullptr;
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

  // Only valid for kByteAddressable: localized byte window within the plane.
  llvm::Value* byte_offset = nullptr;
  uint32_t subview_byte_span = 0;

  TypeId result_type;
};

// Backend-owned SSA carrier for packed values.
//
// Contract:
//   unk == nullptr means the RHS is provably 2-state. No downstream code
//   may coerce nullptr to a zero constant. LowerRhsToPackedRValue is the
//   sole entry point for expression/operand sources;
//   BuildPackedRValueFromRaw is the sole entry point for local-load
//   sources.
//
// Does not carry storage byte size -- that is determined by the
// resolved subview or storage descriptor at the point of use.
// Does not carry storage 4-state status -- that is a property of the
// storage descriptor (PackedStorageView::is_four_state), not the RHS.
struct PackedRValue {
  llvm::Value* val = nullptr;
  llvm::Value* unk = nullptr;
  uint32_t semantic_bits = 0;
};

// Semantic unknown-plane policy. What must happen to the unknown plane.
// Pure fact about the RHS -- no layout or codegen recipe information.
enum class UnknownPlanePolicy {
  kNone,          // 2-state storage. No unknown plane exists.
  kStoreFromRhs,  // 4-state storage, 4-state RHS. Store RHS unknown.
  kClearToZero,   // 4-state storage, 2-state RHS. Unknown must become zero.
};

// Store recipe context. The combination of physical layout and execution
// mode that determines which lowering recipe is cheapest. Each value names
// the structural reason the recipe choice differs.
enum class StoreRecipeContext {
  kSplitPlaneSubview,            // Byte-addr canonical, planes at large offset
  kBitAddressableRmw,            // Full-plane RMW, both planes loaded
  kWholeValueInlineInterleaved,  // Adjacent {val, unk}, single widened store
  kWholeValueRuntimeAssisted,    // Too large for inline, runtime helper
  kDeferredSubview,              // NBA/deferred byte-addressable
  kDeferredFullWidth,            // NBA/deferred full-width masked
};

// Concrete codegen recipe for the unknown plane. Each name describes
// exactly one codegen shape. Selected by the planner from (policy,
// context), not by any executor.
enum class UnknownPlaneLowering {
  kNone,                        // No unknown plane. No-op.
  kWritePlaneAtOffset,          // Store RHS unk at split-plane offset.
  kMergePlaneBitsRmw,           // Merge RHS unk bits in full-plane RMW.
  kFlattenAndStoreWhole,        // Flatten {val, unk} into widened store.
  kMaterializeForRuntime,       // Pass RHS unk to runtime helper / scheduler.
  kConditionalClearAtOffset,    // Branch + conditional zero at split offset.
  kUnconditionalClearAtOffset,  // Unconditional zero store at split offset.
  kMaskedClearBitsRmw,          // Bitwise AND clear in full-plane RMW.
  kFlattenZeroAndStoreWhole,    // Flatten {val, 0} into widened store.
  kMaterializeZeroForRuntime,   // Materialize zero for runtime / scheduler.
};

// Resolved store plan. The planner owns the recipe choice.
// Executors follow the selected recipe mechanically.
struct PackedStorePlan {
  UnknownPlanePolicy unk_policy = UnknownPlanePolicy::kNone;
  UnknownPlaneLowering unk_lowering = UnknownPlaneLowering::kNone;
  // Non-null when policy is kStoreFromRhs.
  llvm::Value* unk_value = nullptr;
  // True when notification will consume the changed predicate.
  // When false, store emitters do not produce the notification-consumed
  // changed result. Unknown-plane storage mutations (including storage-
  // oriented tests like conditional clear) are unaffected by this flag.
  bool needs_changed = false;
};

// Classify subview store recipe context from subview kind.
auto ClassifySubviewRecipeContext(PackedSubviewKind kind) -> StoreRecipeContext;

// Classify whole-value store recipe context from storage layout.
// Asserts structural invariants for inline interleaved classification.
auto ClassifyWholeValueRecipeContext(const PackedStorageView& storage)
    -> StoreRecipeContext;

// Select lowering recipe from (policy, context, notification_deferred).
// Single authority. InternalError on impossible combinations.
// notification_deferred: when true, the changed predicate is dead (not
// consumed by per-store notification). This allows unconditional clear
// recipes where conditional clear would otherwise be selected.
auto SelectUnknownPlaneLowering(
    UnknownPlanePolicy policy, StoreRecipeContext context,
    bool notification_deferred) -> UnknownPlaneLowering;

// Build the complete store plan. Single decision point.
auto BuildPackedStorePlan(
    const PackedStorageView& storage, const PackedRValue& rhs,
    StoreRecipeContext recipe_context, bool notification_deferred)
    -> PackedStorePlan;

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

// Dirty range for packed store notification.
// Both fields are i32 LLVM values (ConstantInt for static, runtime for
// dynamic).
struct PackedDirtyRange {
  llvm::Value* byte_offset = nullptr;
  llvm::Value* byte_size = nullptr;
};

// Derive the notification dirty range from a resolved subview.
// kByteAddressable: localized range from subview byte offset and storage width.
// kBitAddressable: full-slot range (0, 0).
auto GetSubviewDirtyRange(Context& ctx, const PackedSubviewAccess& access)
    -> PackedDirtyRange;

// Emit conditional dirty-mark notification for a packed store.
// Owns the full notification contract: no-op for init/deferred,
// engine-null guard for cross-context, branch on changed, call LyraMarkDirty.
// Shared by localized writes and future whole-value writes.
void EmitPackedStoreNotification(
    Context& ctx, llvm::Value* changed, const PackedStorePolicy& policy,
    const PackedDirtyRange& dirty_range);

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

// Deferred/NBA scheduling policy. Separate from PackedStorePolicy because
// NBA scheduling requires runtime pointers (engine, notify_base) and a
// signal ID for the runtime call, while immediate writes use a different
// notification contract (inline dirty-mark via LyraMarkDirty).
struct PackedNbaPolicy {
  llvm::Value* engine_ptr = nullptr;
  llvm::Value* notify_base_ptr = nullptr;
  SignalIdExpr signal_id;
};

// Localized subview deferred/NBA write.
// Internally selects byte-addressable (narrow overwrite) or bit-addressable
// (full-width masked merge) based on the resolved subview kind.
auto EmitDeferredStoreToPackedSubview(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value,
    const PackedNbaPolicy& policy) -> Result<void>;

// Build a PackedStorageView for whole-value access (no subview path).
// Canonical storage uses the packed ABI layout (GetStorageByteSize /
// FourStateUnknownLaneOffset). Non-canonical uses LLVM alloca layout.
auto BuildWholeValueStorageView(
    Context& ctx, llvm::Value* base_ptr, TypeId type_id, bool is_canonical)
    -> PackedStorageView;

// Build a PackedStorageView for canonical whole-object byte storage with
// full-slot notification semantics. Used for union memcpy and similar
// byte-level storage operations where the type system does not model the
// storage as a language-level packed type, but the canonical storage
// layout is a flat byte region that supports compare+notify.
// The view has a single plane (no unknown plane) because the storage
// is raw bytes, not because the source-language type is 2-state.
auto BuildRawBytesStorageView(llvm::Value* base_ptr, uint32_t byte_size)
    -> PackedStorageView;

// Build PackedStorePolicy from current Context execution contract.
// Single conversion point: DesignStoreMode -> PackedStoreMode, extracts
// engine_ptr, first_dirty_seen, notification policy from Context.
auto BuildStorePolicyFromContext(
    Context& ctx, std::optional<SignalIdExpr> signal_id) -> PackedStorePolicy;

// Plane pointers for runtime-helper interop.
// Returns val_ptr and unk_ptr (null for 2-state) derived from the storage
// descriptor. Callers must not reconstruct plane layout from raw descriptor
// fields. PSV owns the canonical packed layout contract.
struct PackedPlanePointers {
  llvm::Value* val_ptr = nullptr;
  llvm::Value* unk_ptr = nullptr;
};

auto GetPlanePointers(Context& ctx, const PackedStorageView& storage)
    -> PackedPlanePointers;

// Whole-value materialization: load full packed value from storage.
auto MaterializePackedValue(Context& ctx, const PackedStorageView& storage)
    -> Result<PackedRValue>;

// Typed whole-value packed store. The canonical entry point for callers
// that have a typed PackedRValue. PSV performs store, compare, and
// notification internally. Inline vs runtime-helper path selection is
// invisible to callers.
auto StorePackedValue(
    Context& ctx, const PackedStorageView& storage, const PackedRValue& value,
    const PackedStorePolicy& policy) -> Result<void>;

// Whole-value packed store from a canonical byte buffer. The canonical
// entry point for callers that have pre-materialized canonical bytes.
// PSV performs store, compare, and notification internally.
auto StorePackedValueFromCanonicalBytes(
    Context& ctx, const PackedStorageView& storage, llvm::Value* src_bytes_ptr,
    const PackedStorePolicy& policy) -> Result<void>;

// Post-hoc compare+notify for already-written packed storage.
// old_snapshot_ptr holds pre-write canonical bytes. PSV compares old
// vs current storage contents and notifies if changed.
auto NotifyPackedStorageWritten(
    Context& ctx, const PackedStorageView& storage,
    llvm::Value* old_snapshot_ptr, const PackedStorePolicy& policy)
    -> Result<void>;

// Top-level packed load for BitRangeProjection places.
// Runs the full pipeline: extract path -> resolve subview -> emit load ->
// convert to legacy LLVM value form.
auto LoadPackedPlace(
    Context& ctx, mir::PlaceId place_id, llvm::Type* target_type)
    -> Result<llvm::Value*>;

// Build PackedRValue from a raw LLVM value whose type shape is the
// authoritative 2-state/4-state representation.
//
// Contract: struct {iN,iN} means 4-state (both fields extracted), scalar iN
// means 2-state (unk=nullptr). The LLVM type must reflect the SOURCE value's
// declared type, not a target storage type. This is true for:
//   - activation-local shadow loads (alloca type = variable declared type)
//   - operand loads from typed storage
//   - system function returns (always scalar/2-state)
// This is NOT true for values packed by LowerRhsRaw (which forces the target's
// 4-state shape onto a 2-state RHS). Those paths must use
// LowerRhsToPackedRValue.
auto BuildPackedRValueFromRaw(
    Context& ctx, llvm::Value* raw, uint32_t semantic_bits) -> PackedRValue;

// Convert a PackedRValue to legacy LLVM value form ({iN, iN} for 4-state,
// iN for 2-state). Used at the module boundary where callers still expect
// raw llvm::Value* in the old representation.
auto ConvertPackedRValueToLegacyLlvmValue(
    Context& ctx, const PackedRValue& rval, llvm::Type* target_type)
    -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
