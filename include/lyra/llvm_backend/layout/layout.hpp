#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/metadata/design_metadata.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/runtime/body_realization_desc.hpp"

namespace lyra::lowering::mir_to_llvm {

struct SlotInfo;

// Patches for 4-state X-encoding initialization, grouped by store width.
// Each patch contains the byte offset (from base) and the mask to write.
// After memset(0), these patches set the unknown plane bits for scalar 4-state
// fields to encode X values.
struct FourStatePatchTable {
  std::vector<std::pair<uint64_t, uint8_t>> patches_8;
  std::vector<std::pair<uint64_t, uint16_t>> patches_16;
  std::vector<std::pair<uint64_t, uint32_t>> patches_32;
  std::vector<std::pair<uint64_t, uint64_t>> patches_64;

  [[nodiscard]] auto IsEmpty() const -> bool {
    return patches_8.empty() && patches_16.empty() && patches_32.empty() &&
           patches_64.empty();
  }
};

struct SlotIdHash {
  auto operator()(mir::SlotId id) const noexcept -> size_t {
    return std::hash<uint32_t>{}(id.value);
  }
};

struct PlaceIdHash {
  auto operator()(mir::PlaceId id) const noexcept -> size_t {
    return std::hash<uint32_t>{}(id.value);
  }
};

// Root identity key for de-duplicating places in frame layout.
// Multiple PlaceIds with the same root (but different projections) share
// one frame slot.
struct PlaceRootKey {
  mir::PlaceRoot::Kind kind = {};
  int id = 0;

  auto operator==(const PlaceRootKey&) const -> bool = default;
};

struct PlaceRootKeyHash {
  auto operator()(const PlaceRootKey& key) const noexcept -> size_t {
    return std::hash<uint64_t>{}(
        (static_cast<uint64_t>(key.kind) << 32) |
        static_cast<uint32_t>(key.id));
  }
};

// Design-wide state layout artifact.
// DesignState is a Lyra-owned byte arena with two regions:
//   [inline region] [appendix region]
// Inline region: all slots at fixed offsets. kOwnedContainer slots store
// an OwnedStorageHandle here (fixed 16 bytes), not the backing data.
// Appendix region: backing data for kOwnedContainer slots.
struct DesignLayout {
  // Ordered slots, in declaration order.
  std::vector<mir::SlotId> slots;

  // Map from SlotId to slot position index (into slots/offsets/specs vectors).
  std::unordered_map<mir::SlotId, uint32_t, SlotIdHash> slot_to_index;

  // Inline-region byte offset of each slot's inline representation.
  // For kInlineValue: offset of the slot's value bytes.
  // For kOwnedContainer: offset of the OwnedStorageHandle (16 bytes).
  // NOTE: For kOwnedContainer slots, this is NOT the offset of the
  // logical backing data. Use owned_data_offsets for that.
  std::vector<uint64_t> slot_byte_offsets;

  // Total byte size of the design state arena (inline + appendix).
  uint64_t arena_size = 0;

  // Logical storage spec per slot. Describes the slot's semantic storage:
  // For kInlineValue: describes the bytes at slot_byte_offsets[i].
  // For kOwnedContainer: describes the backing data in the appendix
  //   (e.g., ArrayStorageSpec with instance-specific element count).
  //   Does NOT describe the inline handle at slot_byte_offsets[i].
  std::vector<SlotStorageSpec> slot_storage_specs;

  // Arena for child storage specs (array elements, struct fields).
  StorageSpecArena storage_spec_arena;

  // Patches for 4-state X-encoding (byte offsets in inline region).
  // Structurally restricted to inline scalar packed slots only:
  // IsPatchTableEligible requires PackedStorageSpec, which excludes
  // owned-container slots (their specs are ArrayStorageSpec).
  FourStatePatchTable four_state_patches;

  // Appendix-region byte offset for each kOwnedContainer slot's
  // backing data. Empty (nullopt) for kInlineValue slots.
  std::vector<std::optional<uint64_t>> owned_data_offsets;

  // Size of the inline region alone (before appendix).
  uint64_t inline_region_size = 0;
};

// Process frame layout - one per process
// Maps place root identity to field index in ProcessFrameN struct.
// Multiple PlaceIds sharing a root (with different projections) map to
// the same frame slot.
struct FrameLayout {
  // Root types in field order (for 4-state initialization)
  std::vector<TypeId> root_types;
  // Map from root identity to field index
  std::unordered_map<PlaceRootKey, uint32_t, PlaceRootKeyHash> root_to_field;
  // LLVM struct type for ProcessFrameN (built by BuildLayout)
  llvm::StructType* llvm_type = nullptr;
  // Patches for 4-state X-encoding (byte offsets to unknown planes)
  FourStatePatchTable four_state_patches;
};

// Root identity + type pair for alloca allocation in suspension-free processes.
struct AllocaRootInfo {
  PlaceRootKey key;
  TypeId type;
};

// Per-process layout info
struct ProcessLayout {
  size_t process_index = 0;
  FrameLayout frame;
  // LLVM struct type for ProcessStateN: {ProcessStateHeader, ProcessFrameN}
  llvm::StructType* state_type = nullptr;
  // True when the process has Delay/Wait terminators requiring frame storage.
  // When false, locals/temps are emitted as plain allocas that LLVM's
  // mem2reg promotes to SSA registers.
  bool has_suspension = true;
  // Root identity + type for each local/temp that needs an alloca.
  // Populated only when has_suspension is false (suspension-free processes).
  std::vector<AllocaRootInfo> alloca_roots;
};

// Index into module-instance parallel arrays (instance_base_byte_offsets,
// placement.instances, etc.).
struct ModuleIndex {
  uint32_t value = UINT32_MAX;
  static constexpr uint32_t kNone = UINT32_MAX;
  auto operator==(const ModuleIndex&) const -> bool = default;
  auto operator<=>(const ModuleIndex&) const = default;
  explicit operator bool() const {
    return value != kNone;
  }
};

// Cross-boundary metadata contract:
//
// The structs below are produced during layout and consumed later during
// metadata lowering. Arena-local MIR IDs must not appear here without an
// explicit boundary contract:
//
// - Prefer resolved derived data when the consumer does not need MIR identity
//   (for example, ResolvedObservation instead of PlaceId).
// - If later code must perform arena lookup, carry the arena-local ID together
//   with explicit provenance (ModuleIndex).
//
// This keeps wrong-arena lookup unrepresentable in cross-phase metadata.

// Pre-resolved trigger observation for sub-slot narrowing in metadata lowering.
// Computed during layout while the owning MIR arena is still known, so no
// arena-local PlaceId survives into cross-boundary metadata.
struct ResolvedObservation {
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
  uint8_t bit_index = 0;
};

// Entry for a connection process that has been kernelized.
// Instead of generating a per-process LLVM function, these are batched
// into a connection descriptor table evaluated inline by the engine.
//
// Today, all kernelized connections come from port bindings (Phase 2
// of BuildLayout). Module-internal continuous assigns are lowered as
// always_comb processes and kernelized as CombKernels, not connections.
struct ConnectionKernelEntry {
  mir::ProcessId process_id;
  mir::SlotId src_slot;
  mir::SlotId dst_slot;
  mir::SlotId trigger_slot;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  std::optional<ResolvedObservation> trigger_observation;
  metadata::ConnectionKernelOrigin origin =
      metadata::ConnectionKernelOrigin::kPortBinding;
};

// Trigger observation for a comb kernel input slot.
// Stores pre-resolved byte-range data, not arena-local PlaceId.
struct CombTrigger {
  mir::SlotId slot;
  // nullopt => observe full slot.
  std::optional<ResolvedObservation> observation;
};

// Entry for a pure combinational process that can be evaluated inline.
// Unlike connections (memcpy), comb kernels run compiled code but skip
// the full scheduler overhead (subscriptions, queuing, SuspendRecord).
//
// Process identity is the canonical scheduled-process row, assigned once
// during BuildLayout. Later phases consume this directly -- they must not
// reconstruct identity from arena-local IDs.
struct CombKernelEntry {
  // 0-based index into the module-process portion of scheduled_processes,
  // i.e. scheduled_processes[num_init + scheduled_process_index].
  // Assigned once during BuildLayout.
  uint32_t scheduled_process_index = 0;
  std::vector<CombTrigger> triggers;
  // True if the kernel's write slot set overlaps its trigger slot set.
  // Conservative: slot-granular, so sub-slot disjointness is not considered.
  bool has_self_edge = false;
};

// Scheduled process record: pairs a ProcessId with optional module instance.
// Module-owned processes have a valid module_index; design-level processes
// (init, connection) have ModuleIndex::kNone -- they are not bound to any
// module instance.
struct ScheduledProcess {
  mir::ProcessId process_id;
  ModuleIndex module_index;
};

// Full backend layout product.
// Owning process metadata template.
// Canonical form on the backend/layout side. Contains descriptor-ready
// entries and an owned string pool. Emitted directly as LLVM globals
// without any second lowering step.
struct OwnedProcessMetaTemplate {
  std::vector<runtime::ProcessMetaTemplateEntry> entries;
  // NUL-terminated file path strings, '\0' at [0] as empty sentinel.
  std::vector<char> pool;
};

// Includes DesignLayout (design-wide state) plus process layout and
// body-owned specialization addressing.
// Downstream consumer of BuildDesignLayout; constructed by BuildLayout.
struct Layout {
  DesignLayout design;
  std::vector<ProcessLayout> processes;
  // Canonical list of scheduled processes with instance binding.
  // Layout:
  //   [0 .. num_init_processes)            init processes
  //   [num_init_processes .. num_module_process_base)  design-level scheduled
  //   [num_module_process_base .. end)     module processes
  // For design-level processes (init, connection), module_index is kNone.
  std::vector<ScheduledProcess> scheduled_processes;
  size_t num_init_processes = 0;
  // Absolute index into scheduled_processes where module processes begin.
  // Design-level scheduled processes (non-kernelized connections) occupy
  // [num_init_processes .. num_module_process_base).
  size_t num_module_process_base = 0;
  // Connection processes evaluated as batch memcpy
  std::vector<ConnectionKernelEntry> connection_kernel_entries;
  // Pure combinational processes evaluated inline via compiled function
  std::vector<CombKernelEntry> comb_kernel_entries;
  // ProcessStateHeader type: {SuspendRecord, DesignState*}
  llvm::StructType* header_type = nullptr;
  // SuspendRecord type (opaque blob matching C++ struct size)
  llvm::StructType* suspend_record_type = nullptr;

  // Canonical offset accessor: byte offset of instance's slot base.
  auto GetInstanceBaseByteOffset(ModuleIndex idx) const -> uint64_t;
  // Per-instance raw relative byte offsets (body-local slot order).
  // Indexed by module_index, each inner vector has slot_count entries.
  // Used by spec compilation to compute slot stability classification.
  auto GetInstanceRelByteOffsets(ModuleIndex idx) const
      -> const std::vector<uint64_t>&;

  // Number of package-owned slots (before any module instance slots).
  // This is the initial value of the running slot-base counter that the
  // runtime constructor uses. Equals the first module instance's
  // design_state_base_slot, or total slot count if no module instances.
  uint32_t num_package_slots = 0;

  // Pre-computed byte offset of each instance's slot base in DesignState.
  // Parallel to placement.instances.
  std::vector<uint64_t> instance_base_byte_offsets;
  // Per-instance raw relative byte offsets (body-local slot order).
  // Indexed by module_index, each inner vector has slot_count entries.
  std::vector<std::vector<uint64_t>> instance_rel_byte_offsets;

  // Constructor metadata: shared process-state schemas and per-process
  // constructor records. Computed from the process layout loop.

  // Per-schema descriptor for codegen emission.
  // Schema identity is (body_id, proc_within_body) for module processes,
  // or a unique connection-local index for connection processes.
  struct ProcessStateSchemaDesc {
    uint64_t state_size = 0;
    uint64_t state_align = 0;
    bool needs_4state_init = false;
    // Codegen representative: one of the processes sharing this schema.
    // Used only as a codegen anchor to access frame layout for emission.
    // Never used to derive schema identity or grouping.
    size_t representative_process_index = 0;
    // Schema identity components for naming emission and debug assertions.
    // Not the source of truth for grouping (grouping is structural).
    std::optional<mir::ModuleBodyId> body_id;
    // Within-body ordinal index (position in body_processes list).
    std::optional<uint32_t> proc_within_body;
    // For connection schemas: the connection-process semantic index
    // used for naming. This is the canonical naming component for
    // connection schemas.
    std::optional<uint32_t> conn_index;
  };

  std::vector<ProcessStateSchemaDesc> state_schemas;

  // Constructor-side definition artifacts.
  // Body-shaped descriptors consumed by the runtime constructor.

  // Per-body process/schema mapping and metadata template.
  // One entry per unique body_id, ordered by first-seen body_id during
  // schema dedup (deterministic from BFS-sorted elaboration order).
  // Do not reorder.
  struct BodyRealizationInfo {
    mir::ModuleBodyId body_id;
    uint32_t slot_count = 0;
    // Per-process schema indices, ordered by body-local non-final process
    // ordinal (proc_within_body). Dense: every ordinal in [0, size) is
    // present and valid. This ordering matches the body's non-final
    // process list and the compiled function vector for that body.
    std::vector<uint32_t> process_schema_indices;
    // Process metadata template in descriptor-ready canonical form.
    // One entry per proc_within_body, parallel to process_schema_indices.
    // The post-layout metadata template extraction pass is the sole
    // producer.
    OwnedProcessMetaTemplate meta;
  };
  std::vector<BodyRealizationInfo> body_realization_infos;

  // Per-connection schema mapping. Ordered to match connection process
  // scheduling order.
  struct ConnectionRealizationInfo {
    uint32_t schema_index = 0;
  };
  std::vector<ConnectionRealizationInfo> connection_realization_infos;

  // Connection process metadata template. Descriptor-ready canonical form.
  // One entry per connection process, in the same canonical ordering as
  // connection_realization_infos and the EmitConstructorFunction connection
  // loop. The post-layout metadata template extraction pass is the sole
  // producer.
  OwnedProcessMetaTemplate connection_meta;
};

// Type kind for variable inspection (also used in layout)
enum class VarTypeKind : uint8_t {
  kIntegral,  // int, bit, logic (2-state)
  kReal,      // real, shortreal
  kString,    // string
};

// Type info for a design slot (for layout and initialization)
struct SlotTypeInfo {
  VarTypeKind kind = VarTypeKind::kIntegral;
  uint32_t width = 0;
  bool is_signed = false;
  bool is_four_state = false;
};

// Input for slot type information (provided by caller)
struct SlotInfo {
  mir::SlotId slot_id;
  TypeId type_id;            // For LLVM type derivation (actual type)
  SlotTypeInfo type_info{};  // For variable registration (width, signedness)
  mir::StorageShape storage_shape = mir::StorageShape::kInlineValue;
};

// Get the LLVM type for a TypeId. Handles all type kinds: integrals, reals,
// packed types, unpacked arrays/structs/unions, handles
// (string/dynarray/queue). Used by layout computation and observation range
// resolution.
auto GetLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types,
    bool force_two_state) -> llvm::Type*;

// Get the LLVM storage type for an integral type, rounding up to power-of-2.
auto GetLlvmStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type*;

// Get the LLVM struct type for a 4-state value: {iN_storage, iN_storage}
auto GetFourStateStructType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::StructType*;

// Get the LLVM type for passing a MIR type by value.
// Used by: function signature construction, call-site coercion, BindTempValue
// validation.
//
// Returns:
//   - ptr for handle types (string, dynamic array, queue)
//   - double/float for real/shortreal
//   - iN for 2-state packed integrals
//   - {iN, iN} for 4-state packed integrals
//   - nullptr for aggregates (unpacked array/struct/union) - caller uses
//   out-param
//
// Throws InternalError for:
//   - void type (cannot be passed by value)
//   - unsupported type kinds
auto GetLlvmAbiTypeForValue(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types,
    bool force_two_state) -> llvm::Type*;

// Build SlotInfo list from slot descriptors.
// Derives type metadata (kind, width, signedness) for runtime/initialization.
auto BuildSlotInfo(
    std::span<const mir::SlotDesc> slots, const TypeArena& types,
    bool force_two_state) -> std::vector<SlotInfo>;

// Check if a type is "scalar patchable" - i.e., maps to a single 4-state
// storage object (struct {iW, iW} where W is 8/16/32/64).
// These types can be initialized via the patch table optimization.
// Types that return false must use the existing recursive init path.
auto IsScalarPatchable(
    TypeId type_id, const TypeArena& types, bool force_two_state) -> bool;

// Build the design-wide DesignState layout from precomputed slot metadata.
// Computes canonical byte offsets and storage specs for all slots.
// DataLayout is needed only for TargetStorageAbi (pointer size/alignment).
auto BuildDesignLayout(
    const std::vector<SlotInfo>& slots, const TypeArena& types,
    const llvm::DataLayout& dl, bool force_two_state) -> DesignLayout;

// Per-module-instance layout-planning entry.
// Borrowed view into MIR data; valid only for the synchronous BuildLayout call.
// Do not store beyond the call scope.
struct LayoutModulePlan {
  std::span<const mir::ProcessId> body_processes;
  mir::ModuleBodyId body_id;
  uint32_t design_state_base_slot = 0;
  uint32_t slot_count = 0;
};

// Build complete backend layout from narrow planning inputs.
// Pure analysis pass that creates LLVM types but does NOT emit IR.
// Consumes a prebuilt DesignLayout (design-wide state contract).
// TypeArena and force_two_state are needed for frame layout (process-local
// variable type derivation), not for design slot planning.
auto BuildLayout(
    std::span<const mir::ProcessId> init_processes,
    std::span<const mir::ProcessId> connection_processes,
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, const TypeArena& types,
    DesignLayout design_layout, llvm::LLVMContext& ctx,
    const llvm::DataLayout& dl, bool force_two_state) -> Layout;

// Discriminant for byte range resolution results.
// kPrecise: exact byte range known (FieldProjection + const IndexProjection).
// kFullSlot: projection chain not fully resolvable; caller must mark full slot.
enum class RangeKind { kPrecise, kFullSlot };

// Byte range within a design slot, resolved from a MIR Place projection chain.
struct ByteRange {
  RangeKind kind = RangeKind::kFullSlot;
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
  uint8_t bit_index = 0;  // Bit position within byte_offset (edge triggers)
};

// Callback to resolve an Operand to a constant uint64 index value.
// Returns nullopt if the operand cannot be resolved to a compile-time constant.
// Used to look through kUseTemp operands (e.g., cast of a constant).
using IndexResolver =
    std::function<std::optional<uint64_t>(const mir::Operand&)>;

// Resolve a Place's projection chain to a byte range within its root slot.
//
// Walks the canonical storage spec tree in lockstep with the projection
// chain. All byte offsets come from the resolved SlotStorageSpec -- no
// LLVM type layout queries.
//
// Supported projections:
// - FieldProjection: byte offset from StructStorageSpec field table
// - IndexProjection: byte offset from ArrayStorageSpec element stride
// - BitRangeProjection: single-bit only (for edge triggers); produces
//   byte_size=1 with bit_index for the bit position within that byte
//
// Degraded to kFullSlot (conservative full-slot observation/dirty range):
// - Union member projections (UnionStorageSpec does not retain member
//   specs; all union writes are full-slot by the union storage contract)
// - Dynamic (non-constant) array indices
// - Slice, deref, or empty projection chains
//
// The optional resolve_index callback handles kUseTemp operands that are
// ultimately constants (e.g., a cast of a literal).
auto ResolveByteRange(
    const SlotStorageSpec& root_spec, const StorageSpecArena& arena,
    const mir::Place& place, const IndexResolver& resolve_index) -> ByteRange;

}  // namespace lyra::lowering::mir_to_llvm
