#pragma once

#include <span>

#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/storage_construction_recipe.hpp"

namespace lyra::runtime {

// Apply a storage construction recipe to instance-owned storage.
// Interprets the recipe recursively: scalar 4-state init, struct field
// recursion, array element iteration, and owned-container handle
// realization + element init.
void ApplyStorageConstructionRecipeToInstance(
    RuntimeInstance& instance, StorageConstructionRecipeView recipe,
    StorageConstructionRootView roots);

// Apply a storage construction recipe to the design-global arena.
// Used for package/global storage construction.
void ApplyStorageConstructionRecipeToArena(
    std::span<std::byte> arena, StorageConstructionRecipeView recipe,
    StorageConstructionRootView roots);

}  // namespace lyra::runtime
