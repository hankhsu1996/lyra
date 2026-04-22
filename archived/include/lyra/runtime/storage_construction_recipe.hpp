#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace lyra::runtime {

// Storage construction recipe: a flat POD IR for constructor-time
// recursive initialization of design-state storage.
//
// Each op describes a construction action, not a low-level byte write.
// Container realization (handle binding + backing setup) and element
// initialization are first-class semantic operations.
//
// The recipe is emitted as a constant array by the LLVM backend and
// consumed by the runtime constructor. It preserves container structure
// that the old InitPatchEntry model destroyed by flattening.
//
// Three parallel arrays form the complete recipe:
//   ops[]           -- the op tree (parents reference children by index)
//   root_indices[]  -- top-level slot roots into ops[]
//   child_indices[] -- struct child root index side table (StructInitOp
//                      references spans within this array)

enum class StorageConstructionOpKind : uint8_t {
  // Write the unknown-lane mask for a single 4-state packed scalar.
  kScalarFourStateInit,
  // Zero-fill a byte range (unions needing explicit normalization).
  kZeroFill,
  // Initialize an unpacked struct by recursing into child subtrees.
  // Children are referenced via the child_indices side table.
  kStructInit,
  // Initialize a fixed-extent inline unpacked array by iterating elements.
  kInlineArrayInit,
  // Realize an owned container: bind handle, then iterate element init.
  kOwnedContainerConstruct,
};

// Source of element count for array/container construction.
// Introduced as the clean architectural hook for future constructor-time
// extent resolution. Current lowering emits kConstant exclusively.
enum class ExtentSourceKind : uint8_t {
  // Element count is a compile-time constant baked into the recipe.
  kConstant,
};

struct ExtentSource {
  ExtentSourceKind kind;
  uint8_t reserved0 = 0;
  uint16_t reserved1 = 0;
  // For kConstant: the element count.
  uint32_t value = 0;

  static constexpr auto Constant(uint32_t count) -> ExtentSource {
    return ExtentSource{.kind = ExtentSourceKind::kConstant, .value = count};
  }
};

static_assert(sizeof(ExtentSource) == 8);
static_assert(offsetof(ExtentSource, kind) == 0);
static_assert(offsetof(ExtentSource, value) == 4);
static_assert(std::is_trivially_copyable_v<ExtentSource>);
static_assert(std::is_standard_layout_v<ExtentSource>);

// Per-op payload structs. Named fields for each op kind.

struct ScalarFourStateInitOp {
  uint32_t rel_byte_offset;
  uint32_t byte_width;
  uint64_t unknown_mask;
};

static_assert(sizeof(ScalarFourStateInitOp) == 16);
static_assert(offsetof(ScalarFourStateInitOp, rel_byte_offset) == 0);
static_assert(offsetof(ScalarFourStateInitOp, byte_width) == 4);
static_assert(offsetof(ScalarFourStateInitOp, unknown_mask) == 8);

struct ZeroFillOp {
  uint32_t rel_byte_offset;
  uint32_t byte_size;
};

static_assert(sizeof(ZeroFillOp) == 8);

struct StructInitOp {
  uint32_t rel_byte_offset;
  // Index into the recipe's child_indices[] side table where this
  // struct's child root indices begin. child_indices[first_child_index + i]
  // is the ops[] index of the i-th field's subtree root.
  uint32_t first_child_index;
  uint32_t child_count;
};

static_assert(sizeof(StructInitOp) == 12);

struct InlineArrayInitOp {
  uint32_t rel_byte_offset;
  uint32_t element_stride;
  ExtentSource extent;
  // Index into ops[] for the element's construction subtree root.
  // Valid only when has_element_recipe is non-zero.
  uint32_t element_recipe_root;
  // Non-zero if elements need constructor action beyond memset-zero.
  // When zero, only the array structure is described (no element init).
  uint32_t has_element_recipe;
};

static_assert(sizeof(InlineArrayInitOp) == 24);

struct OwnedContainerConstructOp {
  // Inline-region offset of the OwnedStorageHandle.
  uint32_t handle_rel_byte_offset;
  // Body-relative offset of the backing data region.
  uint32_t backing_rel_byte_offset;
  uint32_t element_stride;
  ExtentSource extent;
  // Index into ops[] for the element's construction subtree root.
  // Valid only when has_element_recipe is non-zero.
  uint32_t element_recipe_root;
  // Non-zero if elements need constructor action beyond memset-zero.
  // When zero, only container realization (handle binding) is performed.
  uint32_t has_element_recipe;
};

static_assert(sizeof(OwnedContainerConstructOp) == 28);

// Tagged POD union. The kind discriminator selects which union member
// is active. All members are trivially copyable POD structs.
struct StorageConstructionOp {
  StorageConstructionOpKind kind = StorageConstructionOpKind::kZeroFill;
  uint8_t reserved0 = 0;
  uint16_t reserved1 = 0;

  union {
    ScalarFourStateInitOp scalar_four_state;
    ZeroFillOp zero_fill;
    StructInitOp struct_init;
    InlineArrayInitOp inline_array_init;
    OwnedContainerConstructOp owned_container_construct;
  };

  StorageConstructionOp() : zero_fill{} {
  }

  [[nodiscard]] auto AsScalarFourState() const -> const ScalarFourStateInitOp& {
    return scalar_four_state;
  }
  [[nodiscard]] auto AsZeroFill() const -> const ZeroFillOp& {
    return zero_fill;
  }
  [[nodiscard]] auto AsStructInit() const -> const StructInitOp& {
    return struct_init;
  }
  [[nodiscard]] auto AsInlineArrayInit() const -> const InlineArrayInitOp& {
    return inline_array_init;
  }
  [[nodiscard]] auto AsOwnedContainerConstruct() const
      -> const OwnedContainerConstructOp& {
    return owned_container_construct;
  }
};

static_assert(sizeof(StorageConstructionOp) == 40);
static_assert(alignof(StorageConstructionOp) == 8);
static_assert(offsetof(StorageConstructionOp, kind) == 0);
static_assert(std::is_trivially_copyable_v<StorageConstructionOp>);
static_assert(std::is_standard_layout_v<StorageConstructionOp>);

// Non-owning view of the complete recipe for the C ABI boundary.
struct StorageConstructionRecipeView {
  const StorageConstructionOp* ops = nullptr;
  uint32_t num_ops = 0;
  // Struct child root index side table.
  const uint32_t* child_indices = nullptr;
  uint32_t num_child_indices = 0;
};

// Non-owning view of the root index array for the C ABI boundary.
// Only root-indexed ops are executed at top level. Child ops are
// reachable only through parent references.
struct StorageConstructionRootView {
  const uint32_t* root_indices = nullptr;
  uint32_t num_roots = 0;
};

}  // namespace lyra::runtime
