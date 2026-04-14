#pragma once

#include <cstdint>

#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/decision.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/storage_construction_recipe.hpp"

namespace lyra::runtime {

// Construction program entry: one per module instance, in strict ModuleIndex
// order. The runtime replays BeginBody/AddInstance calls from this table.
struct ConstructionProgramEntry {
  uint32_t body_group;
  uint32_t path_offset;
  uint32_t param_offset;
  uint32_t param_size;
  uint64_t realized_inline_size;
  uint64_t realized_appendix_size;
  // Parent instance index in construction order. UINT32_MAX for top-level.
  // Used by constructor to build children_by_parent for owner-relative
  // connection recipe materialization.
  uint32_t parent_instance_index;
};

// Flat POD reference to one body descriptor package. Mirrors the data
// passed to LyraConstructorBeginBody as individual pointer+count C ABI
// arguments, packed into one struct per body group.
// The runtime reconstructs a BodyDescriptorPackage view from this.
struct BodyDescriptorRef {
  const BodyRealizationDesc* desc;
  const BodyProcessEntry* entries;
  uint32_t num_entries;
  const ProcessMetaTemplateEntry* meta_entries;
  uint32_t num_meta_entries;
  const char* meta_pool;
  uint32_t meta_pool_size;
  const TriggerTemplateEntry* trigger_entries;
  uint32_t num_trigger_entries;
  const TriggerRange* trigger_ranges;
  uint32_t num_trigger_ranges;
  const uint8_t* trigger_shapes;
  const uint8_t* trigger_groupable;
  const CombTemplateEntry* comb_entries;
  uint32_t num_comb_entries;
  const CombKernelDesc* comb_kernels;
  uint32_t num_comb_kernels;
  const ObservableDescriptorEntry* obs_entries;
  uint32_t num_obs_entries;
  const char* obs_pool;
  uint32_t obs_pool_size;
  const StorageConstructionOp* init_recipe;
  uint32_t num_init_recipe_ops;
  const uint32_t* init_recipe_roots;
  uint32_t num_init_recipe_roots;
  const uint32_t* init_recipe_child_indices;
  uint32_t num_init_recipe_child_indices;
  const ParamInitSlotEntry* init_param_slots;
  uint32_t num_init_param_slots;
  // Per body-local process decision metadata tables.
  const DecisionTableDescriptor* decision_tables;
  uint32_t num_decision_tables;
};

}  // namespace lyra::runtime
