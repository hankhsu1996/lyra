#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

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
  mir::PlaceRoot::Kind kind;
  int id;

  auto operator==(const PlaceRootKey&) const -> bool = default;
};

struct PlaceRootKeyHash {
  auto operator()(const PlaceRootKey& key) const noexcept -> size_t {
    return std::hash<uint64_t>{}(
        (static_cast<uint64_t>(key.kind) << 32) |
        static_cast<uint32_t>(key.id));
  }
};

// Design state layout - one per module
// Maps SlotId (the design variable) to field index in DesignState struct.
struct DesignLayout {
  // Ordered slots for DesignState, in declaration order
  std::vector<mir::SlotId> slots;
  // Map from SlotId to field index
  std::unordered_map<mir::SlotId, uint32_t, SlotIdHash> slot_to_field;
  // LLVM struct type for DesignState (built by BuildLayout)
  llvm::StructType* llvm_type = nullptr;
  // Patches for 4-state X-encoding (byte offsets to unknown planes)
  FourStatePatchTable four_state_patches;
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

// Per-process layout info
struct ProcessLayout {
  size_t process_index = 0;
  FrameLayout frame;
  // LLVM struct type for ProcessStateN: {ProcessStateHeader, ProcessFrameN}
  llvm::StructType* state_type = nullptr;
};

// Entry for a connection process that has been kernelized.
// Instead of generating a per-process LLVM function, these are batched
// into a connection descriptor table evaluated inline by the engine.
struct ConnectionKernelEntry {
  mir::ProcessId process_id;
  mir::SlotId src_slot;
  mir::SlotId dst_slot;
  mir::SlotId trigger_slot;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  // Optional observed place for sub-slot trigger narrowing
  std::optional<mir::PlaceId> trigger_observed_place;
};

// Identifies a module variant: a set of module instances whose processes are
// ALL structurally identical (same template assignments for every local process
// index). Assigned by BuildModuleVariants after template analysis.
struct ModuleVariantId {
  uint32_t value;
  static constexpr uint32_t kNone = UINT32_MAX;
  auto operator==(const ModuleVariantId&) const -> bool = default;
};

// Index into Layout::process_templates.
struct TemplateId {
  uint32_t value = UINT32_MAX;
  static constexpr uint32_t kNone = UINT32_MAX;
  auto operator==(const TemplateId&) const -> bool = default;
  auto operator<=>(const TemplateId&) const = default;
  explicit operator bool() const {
    return value != kNone;
  }
};

// Index into module-instance parallel arrays (instance_variant_ids,
// instance_base_byte_offsets, design.instance_slot_ranges, etc.).
struct ModuleIndex {
  uint32_t value = UINT32_MAX;
  static constexpr uint32_t kNone = UINT32_MAX;
  auto operator==(const ModuleIndex&) const -> bool = default;
  auto operator<=>(const ModuleIndex&) const = default;
  explicit operator bool() const {
    return value != kNone;
  }
};

// Index within a module's local process list.
struct LocalProcIndex {
  uint32_t value = UINT32_MAX;
  static constexpr uint32_t kNone = UINT32_MAX;
  auto operator==(const LocalProcIndex&) const -> bool = default;
  auto operator<=>(const LocalProcIndex&) const = default;
  explicit operator bool() const {
    return value != kNone;
  }
};

// Index into Layout::process_ids / Layout::processes.
struct LayoutProcessIndex {
  uint32_t value = UINT32_MAX;
  static constexpr uint32_t kNone = UINT32_MAX;
  auto operator==(const LayoutProcessIndex&) const -> bool = default;
  auto operator<=>(const LayoutProcessIndex&) const = default;
  explicit operator bool() const {
    return value != kNone;
  }
};

// A deduped template function: one per fingerprint group across all instances.
// Does NOT reference a specific variant (a template can span multiple
// variants).
struct ProcessTemplate {
  mir::ProcessId template_process;           // Representative MIR process
  LayoutProcessIndex template_layout_index;  // Index into layout.processes
  std::string func_name;                     // proc_template_{idx}_{fp:x}
  uint32_t template_base_slot_id;            // Representative's slot_begin
  ModuleIndex representative_module_idx;     // For accessing module layout
};

// The "class object" for a module variant: owns slot layout and routes
// processes to deduped template functions.
struct ModuleVariant {
  // Per local_proc_idx: index into process_templates, or nullopt (standalone).
  std::vector<std::optional<TemplateId>> proc_template_ids;
  // Slot layout for this_ptr + rel_offset addressing.
  // Indexed by (slot_id - base_slot_id). Sized to slot_count.
  std::vector<uint64_t> rel_byte_offsets;
};

// Lightweight membership index: "is this process templated?"
// Routing goes through ModuleVariant::proc_template_ids[local_proc_idx].
struct ProcessMembership {
  LocalProcIndex local_proc_idx;  // Position in module's process list
  ModuleIndex module_idx;         // Which module instance owns this process
};

// Routing result: how a process should be emitted.
struct StandaloneRoute {};
struct TemplatedRoute {
  TemplateId template_id;
  ModuleIndex module_idx;
  LocalProcIndex local_proc_idx;
};
using ProcessRoute = std::variant<StandaloneRoute, TemplatedRoute>;

// Complete layout for entire module
struct Layout {
  DesignLayout design;
  std::vector<ProcessLayout> processes;
  // Canonical list of process IDs (single source of truth for ordering)
  // Layout: init processes first (indices 0..num_init_processes-1),
  //         then module processes (indices num_init_processes..)
  std::vector<mir::ProcessId> process_ids;
  // Boundary marker: number of init processes at start of process_ids
  size_t num_init_processes = 0;
  // Connection processes that use the shared runtime kernel
  std::vector<ConnectionKernelEntry> connection_kernel_entries;
  // ProcessStateHeader type: {SuspendRecord, DesignState*}
  llvm::StructType* header_type = nullptr;
  // SuspendRecord type (opaque blob matching C++ struct size)
  llvm::StructType* suspend_record_type = nullptr;

  // Deduped template functions (one per fingerprint group).
  std::vector<ProcessTemplate> process_templates;

  // Canonical routing entrypoint: how should this process be emitted?
  auto RouteProcess(LayoutProcessIndex idx) const -> ProcessRoute;
  // Canonical offset accessor: byte offset of instance's slot base.
  auto GetInstanceBaseByteOffset(ModuleIndex idx) const -> uint64_t;
  // Variant lookup by module instance index.
  auto GetInstanceVariant(ModuleIndex idx) const -> const ModuleVariant&;
  // Variant ID lookup by module instance index.
  auto GetInstanceVariantId(ModuleIndex idx) const -> ModuleVariantId;

 private:
  friend void BuildModuleVariants(
      Layout& layout, const mir::Design& design, const mir::Arena& arena,
      const TypeArena& types, const llvm::DataLayout& dl);

  // Module variants (one per unique template assignment pattern).
  std::vector<ModuleVariant> variants;
  // Parallel to process_ids[num_init_processes..], nullopt = standalone.
  std::vector<std::optional<ProcessMembership>> process_membership;
  // Pre-computed byte offset of each instance's slot base in DesignState.
  // Parallel to instance_variant_ids. Access via GetInstanceBaseByteOffset.
  std::vector<uint64_t> instance_base_byte_offsets;
  // Per-instance module variant ID (parallel to module elements in
  // design.elements). Access via GetInstanceVariantId / GetInstanceVariant.
  std::vector<ModuleVariantId> instance_variant_ids;
};

// Type kind for variable inspection (also used in layout)
enum class VarTypeKind : uint8_t {
  kIntegral,  // int, bit, logic (2-state)
  kReal,      // real, shortreal
  kString,    // string
};

// Type info for a design slot (for layout and initialization)
struct SlotTypeInfo {
  VarTypeKind kind;
  uint32_t width;      // Bit width (64 for real)
  bool is_signed;      // Signedness (integral only)
  bool is_four_state;  // 4-state (logic, reg) vs 2-state (bit, int)
};

// Input for slot type information (provided by caller)
struct SlotInfo {
  mir::SlotId slot_id;
  TypeId type_id;            // For LLVM type derivation (actual type)
  SlotTypeInfo type_info{};  // For variable registration (width, signedness)
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
// Used by: function signature construction, call-site coercion, BindTemp
// invariant.
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

// Build SlotInfo list from design's slots.
// This derives type metadata (kind, width, signedness) for
// runtime/initialization.
auto BuildSlotInfoFromDesign(
    const mir::Design& design, const TypeArena& types, bool force_two_state)
    -> std::vector<SlotInfo>;

// Check if a type is "scalar patchable" - i.e., maps to a single 4-state
// storage object (struct {iW, iW} where W is 8/16/32/64).
// These types can be initialized via the patch table optimization.
// Types that return false must use the existing recursive init path.
auto IsScalarPatchable(
    TypeId type_id, const TypeArena& types, bool force_two_state) -> bool;

// Build complete layout from MIR design.
// This is a pure analysis pass that creates LLVM types but does NOT emit IR.
//
// Parameters:
//   design: MIR design to analyze
//   arena: MIR arena for looking up places
//   types: Type arena for looking up types
//   slots: Ordered list of design slots (declaration order from HIR)
//   ctx: LLVM context for creating struct types
//   dl: LLVM DataLayout for computing byte offsets
//
// Returns:
//   Complete Layout with all struct types created
auto BuildLayout(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types,
    const std::vector<SlotInfo>& slots, llvm::LLVMContext& ctx,
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
// Supports multi-step chains of FieldProjection and const IndexProjection.
// Returns kFullSlot for dynamic indices, BitRange, Slice, Deref, Union, or
// empty projection chains.
//
// The optional resolve_index callback handles kUseTemp operands that are
// ultimately constants (e.g., a cast of a literal). When provided, it is
// called for IndexProjection operands that are not kConst.
auto ResolveByteRange(
    llvm::LLVMContext& llvm_ctx, const llvm::DataLayout& dl,
    const TypeArena& types, const mir::Place& place, TypeId root_type,
    const IndexResolver& resolve_index, bool force_two_state) -> ByteRange;

}  // namespace lyra::lowering::mir_to_llvm
