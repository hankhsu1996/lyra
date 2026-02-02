#pragma once

#include <cstddef>
#include <cstdint>
#include <unordered_map>
#include <vector>

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"

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
  // ProcessStateHeader type: {SuspendRecord, DesignState*}
  llvm::StructType* header_type = nullptr;
  // SuspendRecord type (opaque blob matching C++ struct size)
  llvm::StructType* suspend_record_type = nullptr;
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

// Get the LLVM storage type for an integral type, rounding up to power-of-2.
auto GetLlvmStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type*;

// Get the LLVM struct type for a 4-state value: {iN_storage, iN_storage}
auto GetFourStateStructType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::StructType*;

// Build SlotInfo list from design's slot_table.
// This derives type metadata (kind, width, signedness) for
// runtime/initialization.
auto BuildSlotInfoFromDesign(const mir::Design& design, const TypeArena& types)
    -> std::vector<SlotInfo>;

// Check if a type is "scalar patchable" - i.e., maps to a single 4-state
// storage object (struct {iW, iW} where W is 8/16/32/64).
// These types can be initialized via the patch table optimization.
// Types that return false must use the existing recursive init path.
auto IsScalarPatchable(TypeId type_id, const TypeArena& types) -> bool;

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
    const llvm::DataLayout& dl) -> Layout;

}  // namespace lyra::lowering::mir_to_llvm
