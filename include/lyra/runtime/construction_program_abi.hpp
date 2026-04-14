#pragma once

#include <cstdint>

#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/decision.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/storage_construction_recipe.hpp"

namespace lyra::runtime {

// Construction program entry: one per module instance, in strict ModuleIndex
// order. This entire type is emitted LLVM transport for constructor
// ingestion only. It is never a runtime semantic carrier. All fields
// are consumed exactly once in LyraConstructorRunProgram and do not
// survive into any runtime-facing struct or API.
struct ConstructionProgramEntry {
  uint32_t body_group;
  uint32_t path_offset;
  uint32_t param_offset;
  uint32_t param_size;
  uint64_t realized_inline_size;
  uint64_t realized_appendix_size;
  // Parent instance index in construction order. UINT32_MAX for top-level.
  // Transport-only: consumed in LyraConstructorRunProgram to resolve a
  // live RuntimeInstance* parent pointer. Does not survive into any
  // runtime carrier after ingestion.
  uint32_t parent_instance_index;
  // Structural child edge metadata. Transport-only: consumed in
  // LyraConstructorRunProgram to populate RuntimeInstance::ChildEdge.
  // These are edge metadata for structural relation materialization,
  // not runtime object identity or a general-purpose query key.
  uint32_t child_ordinal_in_coord;
  // Offset and count into the coord_steps pool for this child's
  // RepertoireCoord. UINT32_MAX offset = empty coord (non-generate).
  uint32_t coord_offset;
  uint32_t coord_count;
  // Local instance name offset into path_pool (e.g. "u0").
  // Presentation input for path derivation during assembly.
  // Originates from InstanceEntry::inst_name at compile time -- not
  // recovered from full path strings. Not stored on RuntimeInstance.
  uint32_t inst_name_offset;
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
