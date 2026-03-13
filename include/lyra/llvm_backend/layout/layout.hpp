#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <unordered_map>
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

// Design-wide state layout analysis artifact.
// Defines the LLVM struct shape and slot-to-field mapping for DesignState.
// Computed independently of mir::Design by BuildDesignLayout().
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

// Symbolic trigger observation for a comb kernel input slot.
// Preserves the observed_place from sensitivity analysis for later byte-range
// resolution in metadata lowering.
struct CombTrigger {
  mir::SlotId slot;
  std::optional<mir::PlaceId> observed_place;
};

// Entry for a pure combinational process that can be evaluated inline.
// Unlike connections (memcpy), comb kernels run compiled code but skip
// the full scheduler overhead (subscriptions, queuing, SuspendRecord).
struct CombKernelEntry {
  mir::ProcessId process_id;
  std::vector<CombTrigger> triggers;
  // True if the kernel's write slot set overlaps its trigger slot set.
  // Conservative: slot-granular, so sub-slot disjointness is not considered.
  bool has_self_edge = false;
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

// Scheduled process record: pairs a ProcessId with optional module instance.
// Module-owned processes have a valid module_index; design-level processes
// (init, connection) have ModuleIndex::kNone -- they are not bound to any
// module instance.
struct ScheduledProcess {
  mir::ProcessId process_id;
  ModuleIndex module_index;
};

// Full backend layout product.
// Includes DesignLayout (design-wide state) plus process layout and
// body-owned specialization addressing.
// Downstream consumer of BuildDesignLayout; constructed by BuildLayout.
struct Layout {
  DesignLayout design;
  std::vector<ProcessLayout> processes;
  // Canonical list of scheduled processes with instance binding.
  // Layout: init processes [0..num_init_processes),
  //         then module processes [num_init_processes..)
  // For design-level processes (init, connection), module_index is kNone.
  std::vector<ScheduledProcess> scheduled_processes;
  // Boundary marker: number of init processes at start of scheduled_processes
  size_t num_init_processes = 0;
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
  // Body-owned relative byte offsets: indexed by body_id, contains
  // per-slot relative offsets within the instance's slot region.
  auto GetBodyRelByteOffsets(mir::ModuleBodyId body_id) const
      -> const std::vector<uint64_t>&;

 private:
  friend auto BuildLayout(
      const mir::Design& design, const mir::Arena& arena,
      const TypeArena& types, DesignLayout design_layout,
      llvm::LLVMContext& ctx, const llvm::DataLayout& dl, bool force_two_state)
      -> Layout;

  // Pre-computed byte offset of each instance's slot base in DesignState.
  // Parallel to placement.instances. Access via GetInstanceBaseByteOffset.
  std::vector<uint64_t> instance_base_byte_offsets;
  // Body-owned relative byte offsets, indexed by body_id.value.
  // Each entry contains per-slot relative offsets for that body's slot layout.
  std::vector<std::vector<uint64_t>> body_rel_byte_offsets_;
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

// Build the design-wide DesignState layout from precomputed slot metadata.
// This analysis is independent of mir::Design and defines the LLVM struct
// shape and slot-to-field mapping used by downstream layout/codegen.
auto BuildDesignLayout(
    const std::vector<SlotInfo>& slots, const TypeArena& types,
    llvm::LLVMContext& ctx, const llvm::DataLayout& dl, bool force_two_state)
    -> DesignLayout;

// Build complete layout from MIR design.
// This is a pure analysis pass that creates LLVM types but does NOT emit IR.
// Consumes a prebuilt DesignLayout (design-wide state contract).
auto BuildLayout(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types,
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
