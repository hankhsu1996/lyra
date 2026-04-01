#include "lyra/runtime/storage_construction.hpp"

#include <cstdint>
#include <cstring>
#include <span>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/owned_storage_handle.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/storage_construction_recipe.hpp"

namespace lyra::runtime {

namespace {

auto ResolveExtent(const ExtentSource& extent) -> uint32_t {
  switch (extent.kind) {
    case ExtentSourceKind::kConstant:
      return extent.value;
  }
  throw common::InternalError("ResolveExtent", "unknown extent source kind");
}

void ValidateOpIndex(uint32_t op_index, size_t ops_size, const char* caller) {
  if (op_index >= ops_size) {
    throw common::InternalError(caller, "op index out of range");
  }
}

void ValidateChildIndex(
    uint32_t idx, size_t child_indices_size, const char* caller) {
  if (idx >= child_indices_size) {
    throw common::InternalError(
        caller, "struct child index out of range in child_indices");
  }
}

// Recursively apply one recipe op to instance storage.
void ApplyOpToInstance(
    RuntimeInstance& instance, std::span<const StorageConstructionOp> ops,
    std::span<const uint32_t> child_indices, uint32_t op_index,
    uint32_t base_rel_off) {
  ValidateOpIndex(op_index, ops.size(), "ApplyOpToInstance");
  const auto& op = ops[op_index];

  switch (op.kind) {
    case StorageConstructionOpKind::kScalarFourStateInit: {
      const auto& s = op.AsScalarFourState();
      uint32_t dst_rel = base_rel_off + s.rel_byte_offset;
      auto* dst = ResolveInstanceStorageOffset(
          instance, dst_rel, s.byte_width, "kScalarFourStateInit");
      std::memcpy(dst, &s.unknown_mask, s.byte_width);
      return;
    }

    case StorageConstructionOpKind::kZeroFill: {
      const auto& z = op.AsZeroFill();
      uint32_t dst_rel = base_rel_off + z.rel_byte_offset;
      auto* dst = ResolveInstanceStorageOffset(
          instance, dst_rel, z.byte_size, "kZeroFill");
      std::memset(dst, 0, z.byte_size);
      return;
    }

    case StorageConstructionOpKind::kStructInit: {
      const auto& st = op.AsStructInit();
      uint32_t struct_base = base_rel_off + st.rel_byte_offset;
      for (uint32_t i = 0; i < st.child_count; ++i) {
        uint32_t child_ref_idx = st.first_child_index + i;
        ValidateChildIndex(
            child_ref_idx, child_indices.size(), "ApplyOpToInstance");
        ApplyOpToInstance(
            instance, ops, child_indices, child_indices[child_ref_idx],
            struct_base);
      }
      return;
    }

    case StorageConstructionOpKind::kInlineArrayInit: {
      const auto& arr = op.AsInlineArrayInit();
      if (arr.has_element_recipe == 0) return;
      uint32_t arr_base = base_rel_off + arr.rel_byte_offset;
      uint32_t count = ResolveExtent(arr.extent);
      for (uint32_t i = 0; i < count; ++i) {
        ApplyOpToInstance(
            instance, ops, child_indices, arr.element_recipe_root,
            arr_base + (i * arr.element_stride));
      }
      return;
    }

    case StorageConstructionOpKind::kOwnedContainerConstruct: {
      const auto& c = op.AsOwnedContainerConstruct();
      uint32_t count = ResolveExtent(c.extent);

      auto* handle_addr = ResolveInstanceStorageOffset(
          instance, c.handle_rel_byte_offset, sizeof(OwnedStorageHandle),
          "kOwnedContainerConstruct handle");

      uint64_t backing_total = static_cast<uint64_t>(count) * c.element_stride;
      auto* backing_addr = ResolveInstanceStorageOffset(
          instance, c.backing_rel_byte_offset,
          static_cast<uint32_t>(backing_total),
          "kOwnedContainerConstruct backing");

      OwnedStorageHandle handle{
          .data = backing_addr,
          .byte_size = backing_total,
      };
      std::memcpy(handle_addr, &handle, sizeof(handle));

      if (c.has_element_recipe != 0) {
        for (uint32_t i = 0; i < count; ++i) {
          ApplyOpToInstance(
              instance, ops, child_indices, c.element_recipe_root,
              c.backing_rel_byte_offset + (i * c.element_stride));
        }
      }
      return;
    }
  }

  throw common::InternalError(
      "ApplyOpToInstance", "unknown storage construction op kind");
}

// Helper: resolve a byte offset within the arena and return a pointer.
auto ArenaAddr(
    std::span<std::byte> arena, uint64_t off, uint64_t size, const char* caller)
    -> void* {
  if (off > arena.size() || size > arena.size() - off) {
    throw common::InternalError(caller, "access exceeds arena");
  }
  return arena.data() + static_cast<size_t>(off);
}

// Recursively apply one recipe op to arena storage.
void ApplyOpToArena(
    std::span<std::byte> arena, std::span<const StorageConstructionOp> ops,
    std::span<const uint32_t> child_indices, uint32_t op_index,
    uint64_t base_off) {
  ValidateOpIndex(op_index, ops.size(), "ApplyOpToArena");
  const auto& op = ops[op_index];

  switch (op.kind) {
    case StorageConstructionOpKind::kScalarFourStateInit: {
      const auto& s = op.AsScalarFourState();
      uint64_t dst_off = base_off + s.rel_byte_offset;
      auto* dst = ArenaAddr(arena, dst_off, s.byte_width, "kScalarFourState");
      std::memcpy(dst, &s.unknown_mask, s.byte_width);
      return;
    }

    case StorageConstructionOpKind::kZeroFill: {
      const auto& z = op.AsZeroFill();
      uint64_t dst_off = base_off + z.rel_byte_offset;
      auto* dst = ArenaAddr(arena, dst_off, z.byte_size, "kZeroFill");
      std::memset(dst, 0, z.byte_size);
      return;
    }

    case StorageConstructionOpKind::kStructInit: {
      const auto& st = op.AsStructInit();
      uint64_t struct_base = base_off + st.rel_byte_offset;
      for (uint32_t i = 0; i < st.child_count; ++i) {
        uint32_t child_ref_idx = st.first_child_index + i;
        ValidateChildIndex(
            child_ref_idx, child_indices.size(), "ApplyOpToArena");
        ApplyOpToArena(
            arena, ops, child_indices, child_indices[child_ref_idx],
            struct_base);
      }
      return;
    }

    case StorageConstructionOpKind::kInlineArrayInit: {
      const auto& arr = op.AsInlineArrayInit();
      if (arr.has_element_recipe == 0) return;
      uint64_t arr_base = base_off + arr.rel_byte_offset;
      uint32_t count = ResolveExtent(arr.extent);
      for (uint32_t i = 0; i < count; ++i) {
        ApplyOpToArena(
            arena, ops, child_indices, arr.element_recipe_root,
            arr_base + (static_cast<uint64_t>(i) * arr.element_stride));
      }
      return;
    }

    case StorageConstructionOpKind::kOwnedContainerConstruct: {
      const auto& c = op.AsOwnedContainerConstruct();
      uint32_t count = ResolveExtent(c.extent);

      uint64_t handle_off = base_off + c.handle_rel_byte_offset;
      uint64_t backing_off = base_off + c.backing_rel_byte_offset;
      uint64_t backing_total = static_cast<uint64_t>(count) * c.element_stride;

      auto* backing_addr = ArenaAddr(
          arena, backing_off, backing_total, "kOwnedContainer backing");
      OwnedStorageHandle handle{
          .data = backing_addr,
          .byte_size = backing_total,
      };
      auto* handle_addr = ArenaAddr(
          arena, handle_off, sizeof(OwnedStorageHandle),
          "kOwnedContainer handle");
      std::memcpy(handle_addr, &handle, sizeof(handle));

      if (c.has_element_recipe != 0) {
        for (uint32_t i = 0; i < count; ++i) {
          ApplyOpToArena(
              arena, ops, child_indices, c.element_recipe_root,
              backing_off + (static_cast<uint64_t>(i) * c.element_stride));
        }
      }
      return;
    }
  }

  throw common::InternalError(
      "ApplyOpToArena", "unknown storage construction op kind");
}

}  // namespace

void ApplyStorageConstructionRecipeToInstance(
    RuntimeInstance& instance, StorageConstructionRecipeView recipe,
    StorageConstructionRootView roots) {
  auto ops = std::span(recipe.ops, recipe.num_ops);
  auto children = std::span(recipe.child_indices, recipe.num_child_indices);
  auto root_indices = std::span(roots.root_indices, roots.num_roots);
  for (uint32_t root : root_indices) {
    ApplyOpToInstance(instance, ops, children, root, 0);
  }
}

void ApplyStorageConstructionRecipeToArena(
    std::span<std::byte> arena, StorageConstructionRecipeView recipe,
    StorageConstructionRootView roots) {
  auto ops = std::span(recipe.ops, recipe.num_ops);
  auto children = std::span(recipe.child_indices, recipe.num_child_indices);
  auto root_indices = std::span(roots.root_indices, roots.num_roots);
  for (uint32_t root : root_indices) {
    ApplyOpToArena(arena, ops, children, root, 0);
  }
}

}  // namespace lyra::runtime
