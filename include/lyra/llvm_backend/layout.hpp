#pragma once

#include <cstddef>
#include <cstdint>
#include <unordered_map>
#include <vector>

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

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
  std::vector<mir::ProcessId> process_ids;
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
  uint32_t width;  // Bit width (64 for real)
  bool is_signed;  // Signedness (integral only)
};

// Input for slot type information (provided by caller)
struct SlotInfo {
  mir::SlotId slot_id;
  TypeId type_id;            // For LLVM type derivation (actual type)
  SlotTypeInfo type_info{};  // For variable registration (width, signedness)
};

// Build SlotInfo list from design's slot_table.
// This derives type metadata (kind, width, signedness) for
// runtime/initialization.
auto BuildSlotInfoFromDesign(const mir::Design& design, const TypeArena& types)
    -> std::vector<SlotInfo>;

// Build complete layout from MIR design.
// This is a pure analysis pass that creates LLVM types but does NOT emit IR.
//
// Parameters:
//   design: MIR design to analyze
//   arena: MIR arena for looking up places
//   types: Type arena for looking up types
//   slots: Ordered list of design slots (declaration order from HIR)
//   ctx: LLVM context for creating struct types
//
// Returns:
//   Complete Layout with all struct types created
auto BuildLayout(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types,
    const std::vector<SlotInfo>& slots, llvm::LLVMContext& ctx) -> Layout;

}  // namespace lyra::lowering::mir_to_llvm
