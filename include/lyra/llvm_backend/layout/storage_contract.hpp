#pragma once

#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::lowering::mir_to_llvm {

// Align a value up to the given alignment boundary.
// Alignment must be non-zero.
inline auto AlignUp(uint64_t value, uint64_t align) -> uint64_t {
  return (value + align - 1) / align * align;
}

// Narrow uint64_t to uint32_t with overflow check.
// Design invariant: storage sizes, field offsets, and patch offsets within
// individual specs fit in 32 bits. Arena-level offsets use uint64_t.
inline auto NarrowToU32(uint64_t value, const char* context) -> uint32_t {
  if (value > UINT32_MAX) {
    throw common::InternalError(context, "value exceeds uint32_t range");
  }
  return static_cast<uint32_t>(value);
}

// Canonical storage byte size for a packed integer lane.
//
// Lyra ABI rule: packed values are stored in power-of-2 rounded integer
// containers up to 64 bits. Above 64 bits, storage is byte-aligned
// ceil(bit_width/8). This is the authoritative storage contract.
auto GetStorageByteSize(uint32_t bit_width) -> uint32_t;

// Canonical storage alignment for a packed integer lane.
//
// Lyra ABI rule: packed alignment is min(lane_byte_size, 8). This caps
// alignment at 8 bytes (64-bit natural alignment) for wide packed values,
// avoiding non-power-of-two or excessively large alignment for widths > 64.
auto GetStorageAlignment(uint32_t bit_width) -> uint32_t;

// Byte offset of the unknown lane within 4-state packed storage.
// Single source of truth for the 4-state lane layout rule:
// known lane at offset 0, unknown lane at offset = lane_byte_size.
auto FourStateUnknownLaneOffset(uint32_t bit_width) -> uint32_t;

// Total byte size for 4-state packed storage (both lanes).
auto FourStateTotalByteSize(uint32_t bit_width) -> uint32_t;

// Stable index into a StorageSpecArena. Used instead of raw pointers
// to avoid invalidation when the arena grows.
struct StorageSpecId {
  uint32_t value;
};

// Every storage spec variant carries these resolved layout facts.
// No recursive querying or external context needed after construction.
struct StorageLayoutFacts {
  uint32_t total_byte_size;
  uint32_t alignment;
};

// Storage for packed bitvectors (scalars and packed aggregates).
// Lyra ABI rule: all packed types are flattened into a single bitvector
// of PackedBitWidth() and stored in power-of-2 rounded integer storage.
struct PackedStorageSpec {
  StorageLayoutFacts layout;
  uint32_t bit_width;
  bool is_four_state;

  [[nodiscard]] auto LaneByteSize() const -> uint32_t {
    return GetStorageByteSize(bit_width);
  }
  [[nodiscard]] auto UnknownLaneOffset() const -> uint32_t {
    return is_four_state ? FourStateUnknownLaneOffset(bit_width) : 0;
  }
};

// Storage for IEEE 754 floating-point values.
struct FloatStorageSpec {
  StorageLayoutFacts layout;
};

// Storage for fixed-size unpacked arrays.
// N repetitions of element canonical storage at constant stride.
struct ArrayStorageSpec {
  StorageLayoutFacts layout;
  uint32_t element_count;
  uint32_t element_stride;
  StorageSpecId element_spec_id;
};

// One field within an unpacked struct's canonical storage.
struct StructFieldSpec {
  uint32_t byte_offset;
  StorageSpecId field_spec_id;
};

// Storage for unpacked structs.
// Fields in declaration order. Field alignment follows Lyra canonical
// storage alignment rules (packed: min(lane_size, 8); handles: target
// pointer alignment; aggregates: recursive). All padding bytes are part
// of the canonical contract and must be normalized to zero on every write.
struct StructStorageSpec {
  StorageLayoutFacts layout;
  std::vector<StructFieldSpec> fields;
};

// Storage for unpacked unions.
// Size = max member alloc size, aligned to max member alignment.
// Normalization rule: on every write, ALL bytes of the union storage are
// written. Inactive bytes (beyond the active member's size) are zeroed.
// This ensures bytewise change detection (memcmp) is correct without
// partial-field reasoning.
//
// Member specs are not retained individually. has_four_state_content
// summarizes whether any member contains 4-state packed content,
// computed during resolution.
struct UnionStorageSpec {
  StorageLayoutFacts layout;
  // True if any union member contains 4-state packed content.
  // Computed during ResolveStorageSpec from member specs.
  bool has_four_state_content;
};

// Distinguishes handle sub-kinds for metadata classification.
// The storage layout is identical (pointer-width slot), but runtime
// metadata needs to distinguish string from container handles.
enum class HandleKind : uint8_t {
  kString,
  kContainer,
};

// Storage for managed handles (string, dynamic array, queue, associative
// array). Committed through separate handle paths, not through canonical
// storage materialization.
struct HandleStorageSpec {
  StorageLayoutFacts layout;
  HandleKind kind;
};

// The fully resolved canonical storage specification.
using SlotStorageData = std::variant<
    PackedStorageSpec, FloatStorageSpec, ArrayStorageSpec, StructStorageSpec,
    UnionStorageSpec, HandleStorageSpec>;

struct SlotStorageSpec {
  SlotStorageData data;

  [[nodiscard]] auto TotalByteSize() const -> uint32_t;
  [[nodiscard]] auto Alignment() const -> uint32_t;
};

// Returns true if a resolved storage spec is eligible for X-initialization
// via the scalar patch table. The full rule:
// - Must be PackedStorageSpec (not aggregate/handle/float)
// - Must be 4-state
// - Lane byte size must be 1, 2, 4, or 8 (patch table entry sizes)
// Slots that fail this check use recursive default initialization instead.
auto IsPatchTableEligible(const SlotStorageSpec& spec) -> bool;

// Arena for storage specs. Child specs (array elements, struct fields)
// are allocated here. References use StorageSpecId (index), not pointers,
// so the arena can grow without invalidation.
class StorageSpecArena {
 public:
  auto Alloc(SlotStorageSpec spec) -> StorageSpecId {
    auto id = StorageSpecId{static_cast<uint32_t>(specs_.size())};
    specs_.push_back(std::move(spec));
    return id;
  }

  [[nodiscard]] auto Get(StorageSpecId id) const -> const SlotStorageSpec& {
    if (id.value >= specs_.size()) {
      throw common::InternalError(
          "StorageSpecArena::Get",
          std::format(
              "spec id {} out of range (size {})", id.value, specs_.size()));
    }
    return specs_[id.value];
  }

  [[nodiscard]] auto Size() const -> uint32_t {
    return static_cast<uint32_t>(specs_.size());
  }

 private:
  std::vector<SlotStorageSpec> specs_;
};

// Target ABI facts needed for storage resolution.
// Provided once at layout time, not re-queried per type.
struct TargetStorageAbi {
  uint32_t pointer_byte_size;
  uint32_t pointer_alignment;
};

// Resolved storage mode for a type. Determined upstream by the layout/
// specialization layer from TypeId + compilation policy (force_two_state).
// The storage contract consumes this, not raw policy booleans.
enum class StorageMode : uint8_t {
  kNormal,    // 4-state for logic types, 2-state for bit/int types
  kTwoState,  // Force all packed types to 2-state representation
};

// Resolve a TypeId into a fully specified SlotStorageSpec.
//
// storage_mode is determined upstream (layout/specialization decides).
// The storage contract does not accept raw policy booleans.
//
// For recursive types (arrays of structs, etc.), child specs are allocated
// into the provided arena. The arena must outlive all returned specs.
auto ResolveStorageSpec(
    TypeId type_id, const TypeArena& types, StorageMode mode,
    const TargetStorageAbi& target, StorageSpecArena& arena) -> SlotStorageSpec;

// Query whether a storage spec contains any 4-state content.
// Recurses through arrays and structs via the arena.
// Used to determine whether a slot needs recursive 4-state default
// initialization after memset zero.
auto HasFourStateContent(
    const SlotStorageSpec& spec, const StorageSpecArena& arena) -> bool;

}  // namespace lyra::lowering::mir_to_llvm
