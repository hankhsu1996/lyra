#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <type_traits>
#include <unordered_map>
#include <vector>

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/kernel_types.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/wait_site.hpp"

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

// SlotIdHash is defined in kernel_types.hpp (included above).

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

  // Canonical storage ownership. Parallel to slots.
  // For storage owners: equals the slot's own id.
  // For forwarded aliases: equals the canonical storage owner's slot id.
  // Always an actual slot id; no sentinels.
  std::vector<uint32_t> storage_owner_slot_id;

  // Check if a slot is in this layout.
  [[nodiscard]] auto ContainsSlot(mir::SlotId slot_id) const -> bool;

  // Check if a slot is a storage owner (not a forwarded alias).
  // Throws InternalError if slot_id is not in the layout.
  [[nodiscard]] auto IsStorageOwner(mir::SlotId slot_id) const -> bool;

  // Get the canonical storage owner slot id for any slot.
  // Throws InternalError if slot_id is not in the layout.
  [[nodiscard]] auto GetStorageOwnerSlotId(mir::SlotId slot_id) const
      -> mir::SlotId;

  // Resolve to the storage owner's layout row index.
  // Throws InternalError if slot_id or its owner is not in the layout.
  [[nodiscard]] auto ResolveStorageOwnerIndex(mir::SlotId slot_id) const
      -> uint32_t;

  // Get the byte offset of the canonical storage for this slot.
  // Resolves through storage ownership: aliases return the owner's offset.
  [[nodiscard]] auto GetStorageByteOffset(mir::SlotId slot_id) const
      -> uint64_t;

  // Get the storage spec of the canonical storage for this slot.
  // Resolves through storage ownership: aliases return the owner's spec.
  [[nodiscard]] auto GetStorageSpec(mir::SlotId slot_id) const
      -> const SlotStorageSpec&;
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
// ConnectionKernelEntry, ResolvedObservation, and SlotIdHash are defined
// in kernel_types.hpp (included above).

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

// Owning trigger metadata template.
//
// Used for both module-body and connection process trigger templates.
//
// Slot-id contract: each entry's slot_id is either body-relative
// (kTriggerTemplateFlagDesignGlobal clear) or design-global
// (kTriggerTemplateFlagDesignGlobal set). Body templates may contain
// both kinds (module-local slots are body-relative, design-global
// slots from hierarchical accesses are already absolute). Connection
// templates are always design-global. The constructor applies
// slot-base relocation only for entries without the design-global flag.
//
// Indexing contract: entries are keyed by explicit local process ordinal
// within the owning template domain (proc_within_body for body templates,
// connection scheduling order for connection templates).
//
// Ordering contract: entries within each proc_ranges slice are in MIR
// Wait trigger declaration order. Constructor realization preserves
// this order exactly in realized word tables.
struct OwnedTriggerTemplate {
  std::vector<runtime::TriggerTemplateEntry> entries;
  // Per local-process-ordinal: (start, count) into entries.
  std::vector<runtime::TriggerRange> proc_ranges;
  // Per local-process-ordinal: trigger shape kind (WaitShapeKind as u8).
  // uint8_t transport type avoids reinterpret_cast at the C ABI boundary.
  std::vector<uint8_t> proc_shapes;
  // Per local-process-ordinal: pre-computed stage-1 groupability.
  // uint8_t (not bool) for contiguous storage compatible with span.
  std::vector<uint8_t> proc_groupable;
};

// Owning comb kernel metadata template.
//
// Slot-id contract: each entry's slot_id is either body-relative
// (kCombTemplateFlagDesignGlobal clear) or design-global
// (kCombTemplateFlagDesignGlobal set). Body comb templates may contain
// both kinds. The constructor applies slot-base relocation only for
// entries without the design-global flag.
//
// Ordering contract: kernels are ordered by proc_within_body. Within
// each kernel, trigger entries are in canonical order: per-slot
// observation merging followed by sort by final design-global slot_id.
// The final global id is computed
// from any representative instance's base_slot for module-local signals;
// this is valid because the per-body entry order is invariant under a
// constant base-slot translation across all instances of the same body.
struct OwnedCombTemplate {
  std::vector<runtime::CombTemplateEntry> entries;
  std::vector<runtime::CombKernelDesc> kernels;
};

// Owning observable descriptor template.
//
// One canonical compile-time source of truth for observability-facing
// slot/signal facts. Used for both body-owned and package/global
// descriptor collections. The constructor expands body templates
// per-instance and copies package templates directly to produce realized
// slot metadata and trace signal metadata tables.
struct OwnedObservableDescriptorTemplate {
  std::vector<runtime::ObservableDescriptorEntry> entries;
  // NUL-terminated names used by observable descriptors:
  // - body-owned templates store local signal names
  // - package/global templates store package-qualified names
  // pool[0] == '\0' is the empty sentinel.
  std::vector<char> pool;
};

// Scoped signal identity key. Used for scope-aware slot identity
// in comb kernel analysis and template extraction to avoid conflating
// module-local and design-global slots with the same numeric id.
struct ScopedSignalKey {
  mir::SignalRef::Scope scope = mir::SignalRef::Scope::kModuleLocal;
  uint32_t id = 0;
  auto operator==(const ScopedSignalKey&) const -> bool = default;
};

struct ScopedSignalKeyHash {
  auto operator()(const ScopedSignalKey& k) const noexcept -> size_t {
    auto scope_val =
        static_cast<std::underlying_type_t<mir::SignalRef::Scope>>(k.scope);
    return std::hash<uint64_t>{}(
        (static_cast<uint64_t>(scope_val) << 32) | k.id);
  }
};

// Body-relative comb kernel analysis result.
//
// Returned by AnalyzeCombKernel. OwnedCombTemplate construction is
// the sole consumer. No other code path may re-derive kernel facts
// from MIR after this result is produced.
struct CombAnalysisResult {
  struct TriggerFact {
    mir::SignalRef signal = {};
    std::optional<mir::PlaceId> observed_place;
  };
  std::vector<TriggerFact> triggers;
  bool has_self_edge = false;
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
    // Trigger metadata template. Flat entries + per-process range table.
    // The post-layout template extraction pass is the sole producer.
    OwnedTriggerTemplate triggers;
    // Comb kernel metadata template. Flat entries + per-kernel descriptors.
    // The post-layout template extraction pass is the sole producer.
    OwnedCombTemplate comb;
    // Observable descriptor template. Flat entries + local-name pool.
    // The post-layout descriptor extraction pass is the sole producer.
    OwnedObservableDescriptorTemplate observable_descriptors;
  };
  std::vector<BodyRealizationInfo> body_realization_infos;

  // Per-connection codegen trigger fact. Intentionally reduced payload
  // from ProcessTriggerFact (process.hpp): only the fields needed for
  // trigger template extraction. Stored on the connection realization
  // record without requiring process.hpp include.
  //
  // Connection trigger signals are always design-global by construction:
  // connection processes are design-level (not module-scoped), so all
  // signal refs are kDesignGlobal. Template extraction sets
  // kTriggerTemplateFlagDesignGlobal on every entry unconditionally.
  struct ConnectionTriggerFact {
    mir::SignalRef signal = {};
    common::EdgeKind edge = common::EdgeKind::kAnyChange;
    bool has_observed_place = false;
  };
  struct ConnectionTriggerResult {
    std::vector<ConnectionTriggerFact> triggers;
    runtime::WaitShapeKind shape = runtime::WaitShapeKind::kStatic;
  };

  // Per-connection schema mapping and trigger codegen result.
  // Ordered to match connection process scheduling order.
  struct ConnectionRealizationInfo {
    uint32_t schema_index = 0;
    // Stable process identity for Phase 5 connection-ordinal verification.
    // Set during BuildLayout from the scheduled_processes entry.
    mir::ProcessId process_id = {};
    // Trigger codegen result, populated during Phase 5.
    // Consumed by the post-layout trigger template extraction pass.
    std::optional<ConnectionTriggerResult> trigger;
  };
  std::vector<ConnectionRealizationInfo> connection_realization_infos;

  // Connection-process template package. Groups all connection-scoped
  // owning template data. Parallel to BodyRealizationInfo for body
  // templates.
  //
  // All fields are indexed by connection process scheduling order:
  // the canonical ordering produced by BuildLayout Phase 2 (the
  // connection scheduling loop), which also produces
  // connection_realization_infos.
  //
  // Identity contract:
  // - meta.entries[i] describes the i-th connection process
  // - triggers.proc_ranges[i] describes the i-th connection's triggers
  // - The i-th AddConnection() call at constructor time consumes both
  struct ConnectionTemplates {
    // Process metadata template. The post-layout metadata template
    // extraction pass is the sole producer.
    OwnedProcessMetaTemplate meta;
    // Trigger metadata template. The post-layout template extraction
    // pass is the sole producer.
    OwnedTriggerTemplate triggers;
  };
  ConnectionTemplates connection_templates;

  // Design-wide package/global observable descriptor template.
  // All entries are absolute (non-relocating). Realized in the constructor
  // prelude before any body-instance expansion.
  OwnedObservableDescriptorTemplate package_observable_descriptors;
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

class ForwardingMap;

// Build the design-wide DesignState layout from precomputed slot metadata.
// Forwarding map controls storage ownership: alias slots share the canonical
// source's byte offset. Pass an empty ForwardingMap for the pre-forwarding
// layout (used by connection collection for observation resolution).
// DataLayout is needed only for TargetStorageAbi (pointer size/alignment).
auto BuildDesignLayout(
    const std::vector<SlotInfo>& slots, const TypeArena& types,
    const llvm::DataLayout& dl, bool force_two_state,
    const ForwardingMap& forwarding) -> DesignLayout;

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
// Consumes a prebuilt DesignLayout and pre-collected connection kernels.
// BuildLayout does not collect or canonicalize connection kernels;
// that is a pipeline-stage responsibility upstream of layout.
// TypeArena and force_two_state are needed for frame layout (process-local
// variable type derivation), not for design slot planning.
auto BuildLayout(
    std::span<const mir::ProcessId> init_processes,
    std::vector<ConnectionKernelEntry> precollected_connection_kernels,
    std::vector<mir::ProcessId> non_kernelized_connection_processes,
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

// Body-relative comb kernel analysis.
// Returns nullopt if the process does not qualify as a comb kernel.
// Result is body-relative: signal refs stay kModuleLocal, self-edge
// detection uses body-relative slot overlap. OwnedCombTemplate
// construction is the sole consumer of this result.
auto AnalyzeCombKernel(const mir::Process& process, const mir::Arena& arena)
    -> std::optional<CombAnalysisResult>;

// Resolve a trigger observation to a byte range within a design slot.
// Returns nullopt if the projection cannot be precisely resolved
// (degraded to full-slot observation).
auto ResolveObservation(
    const mir::Arena& arena, const DesignLayout& design_layout,
    mir::SlotId design_global_slot, mir::PlaceId place_id)
    -> std::optional<ResolvedObservation>;

}  // namespace lyra::lowering::mir_to_llvm
