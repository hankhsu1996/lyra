#include "lyra/llvm_backend/storage_construction_recipe_builder.hpp"

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/runtime/storage_construction_recipe.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto ComputeUnknownMask(uint32_t bit_width) -> uint64_t {
  if (bit_width >= 64) return ~uint64_t{0};
  return (uint64_t{1} << bit_width) - 1;
}

auto Emit(
    std::vector<runtime::StorageConstructionOp>& ops,
    runtime::StorageConstructionOp op) -> uint32_t {
  auto idx = static_cast<uint32_t>(ops.size());
  ops.push_back(op);
  return idx;
}

// Recursively build a construction recipe subtree for a storage spec.
//
// Offset contract: rel_off is the offset of this spec's storage relative
// to the enclosing parent's base. For array elements, rel_off is 0 (the
// array op applies element_stride * i as the base). For struct fields,
// rel_off is the field's byte_offset within the struct. The interpreter
// adds the parent-supplied base to each op's rel_byte_offset.
//
// Returns the op index of the root of this subtree, or nullopt if
// this spec needs no constructor action beyond memset-zero.
auto BuildRecipeImpl(
    const SlotStorageSpec& spec, const StorageSpecArena& arena,
    uint32_t rel_off, std::vector<runtime::StorageConstructionOp>& ops,
    std::vector<uint32_t>& child_indices) -> std::optional<uint32_t> {
  return std::visit(
      common::Overloaded{
          [&](const PackedStorageSpec& packed) -> std::optional<uint32_t> {
            if (!packed.is_four_state) return std::nullopt;

            runtime::StorageConstructionOp op{};
            op.kind = runtime::StorageConstructionOpKind::kScalarFourStateInit;
            op.scalar_four_state = {
                .rel_byte_offset = rel_off + packed.UnknownLaneOffset(),
                .byte_width = packed.LaneByteSize(),
                .unknown_mask = ComputeUnknownMask(packed.bit_width),
            };
            return Emit(ops, op);
          },

          [&](const StructStorageSpec& st) -> std::optional<uint32_t> {
            // Build each field's subtree. Field offsets are relative
            // to the struct base, NOT accumulated with rel_off.
            // The struct op's rel_byte_offset carries the struct's
            // own offset; the interpreter adds that as the base
            // before recursing into children.
            std::vector<std::optional<uint32_t>> field_roots;
            field_roots.reserve(st.fields.size());
            for (const auto& field : st.fields) {
              const auto& child_spec = arena.Get(field.field_spec_id);
              field_roots.push_back(BuildRecipeImpl(
                  child_spec, arena, field.byte_offset, ops, child_indices));
            }

            // Count non-nullopt children. If all fields need no init,
            // the struct itself needs no init.
            uint32_t active_count = 0;
            for (const auto& r : field_roots) {
              if (r.has_value()) ++active_count;
            }
            if (active_count == 0) return std::nullopt;

            // Record child root indices in the side table.
            auto first_child = static_cast<uint32_t>(child_indices.size());
            for (const auto& r : field_roots) {
              if (r.has_value()) {
                child_indices.push_back(*r);
              }
            }

            runtime::StorageConstructionOp op{};
            op.kind = runtime::StorageConstructionOpKind::kStructInit;
            op.struct_init = {
                .rel_byte_offset = rel_off,
                .first_child_index = first_child,
                .child_count = active_count,
            };
            return Emit(ops, op);
          },

          [&](const ArrayStorageSpec& arr) -> std::optional<uint32_t> {
            // Build element recipe at offset 0. The array op applies
            // base + i * stride before recursing.
            const auto& elem_spec = arena.Get(arr.element_spec_id);
            auto elem_root =
                BuildRecipeImpl(elem_spec, arena, 0, ops, child_indices);

            // If element needs no init, the array needs no init.
            if (!elem_root.has_value()) return std::nullopt;

            runtime::StorageConstructionOp op{};
            op.kind = runtime::StorageConstructionOpKind::kInlineArrayInit;
            op.inline_array_init = {
                .rel_byte_offset = rel_off,
                .element_stride = arr.element_stride,
                .extent = runtime::ExtentSource::Constant(arr.element_count),
                .element_recipe_root = *elem_root,
                .has_element_recipe = 1,
            };
            return Emit(ops, op);
          },

          [](const FloatStorageSpec&) -> std::optional<uint32_t> {
            // Float storage is zero-initialized by memset. No action.
            return std::nullopt;
          },

          [&](const UnionStorageSpec& uni) -> std::optional<uint32_t> {
            // Unions use zero-fill normalization.
            runtime::StorageConstructionOp op{};
            op.kind = runtime::StorageConstructionOpKind::kZeroFill;
            op.zero_fill = {
                .rel_byte_offset = rel_off,
                .byte_size = uni.layout.total_byte_size,
            };
            return Emit(ops, op);
          },

          [](const HandleStorageSpec&) -> std::optional<uint32_t> {
            // Handle slots (string, dynamic array) are initialized
            // through separate runtime paths. No recipe action.
            return std::nullopt;
          },
      },
      spec.data);
}

}  // namespace

auto BuildStorageConstructionRecipeForSlot(
    const SlotStorageSpec& spec, const StorageSpecArena& arena,
    uint32_t rel_byte_offset, bool is_owned_container,
    uint32_t handle_rel_byte_offset, uint32_t backing_rel_byte_offset,
    std::vector<runtime::StorageConstructionOp>& out_ops,
    std::vector<uint32_t>& out_child_indices) -> std::optional<uint32_t> {
  if (is_owned_container) {
    const auto* arr = std::get_if<ArrayStorageSpec>(&spec.data);
    if (arr == nullptr) {
      throw common::InternalError(
          "BuildStorageConstructionRecipeForSlot",
          "owned container slot must have ArrayStorageSpec");
    }

    // Element recipe at offset 0. The container op applies
    // backing_base + i * stride before recursing.
    const auto& elem_spec = arena.Get(arr->element_spec_id);
    auto elem_root =
        BuildRecipeImpl(elem_spec, arena, 0, out_ops, out_child_indices);

    // Owned containers always need a recipe (handle must be realized),
    // even if elements need no four-state init.
    runtime::StorageConstructionOp op{};
    op.kind = runtime::StorageConstructionOpKind::kOwnedContainerConstruct;
    op.owned_container_construct = {
        .handle_rel_byte_offset = handle_rel_byte_offset,
        .backing_rel_byte_offset = backing_rel_byte_offset,
        .element_stride = arr->element_stride,
        .extent = runtime::ExtentSource::Constant(arr->element_count),
        .element_recipe_root = elem_root.value_or(0),
        .has_element_recipe = elem_root.has_value() ? 1U : 0U,
    };
    return Emit(out_ops, op);
  }

  return BuildRecipeImpl(
      spec, arena, rel_byte_offset, out_ops, out_child_indices);
}

}  // namespace lyra::lowering::mir_to_llvm
