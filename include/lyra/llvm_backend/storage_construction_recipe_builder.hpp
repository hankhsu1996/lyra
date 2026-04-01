#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/runtime/storage_construction_recipe.hpp"

namespace lyra::lowering::mir_to_llvm {

// Build a storage construction recipe subtree for one slot.
//
// For inline-value slots, the recipe describes recursive default
// initialization (4-state X-encoding for packed scalars, recursive
// field/element init for structs/arrays, zero-fill for unions).
//
// For owned-container slots, the recipe describes handle construction
// (binding the OwnedStorageHandle to the backing region) followed by
// recursive element initialization.
//
// Returns the root op index for the constructed subtree, or nullopt
// if the slot needs no constructor action beyond memset-zero.
// Appends ops to out_ops and struct child indices to out_child_indices.
auto BuildStorageConstructionRecipeForSlot(
    const SlotStorageSpec& spec, const StorageSpecArena& arena,
    uint32_t rel_byte_offset, bool is_owned_container,
    uint32_t handle_rel_byte_offset, uint32_t backing_rel_byte_offset,
    std::vector<runtime::StorageConstructionOp>& out_ops,
    std::vector<uint32_t>& out_child_indices) -> std::optional<uint32_t>;

}  // namespace lyra::lowering::mir_to_llvm
