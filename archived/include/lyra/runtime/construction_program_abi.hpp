#pragma once

#include <cstdint>

#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/decision.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/storage_construction_recipe.hpp"

namespace lyra::runtime {

// Runtime scope tree node kind carried in ConstructionProgramEntry.
// Typed at the C++ source-of-truth; emitted and consumed through an i32
// at the LLVM ABI boundary. Underlying type is fixed so struct layout
// and LLVM struct type stay in lockstep.
enum class ConstructionNodeKind : uint32_t {
  kInstance = 0,
  kGenerate = 1,
};

// Construction program entry: one per scope node (module instance or
// generate scope), in strict parent-before-child order. Emitted LLVM
// transport for constructor ingestion only. All fields are consumed
// exactly once in LyraConstructorRunProgram and do not survive into
// any runtime-facing struct or API.
//
// Hierarchical paths are NOT transported. The constructor derives each
// scope's path from its parent's path_storage + label via BuildScopePath,
// so only the parent-relative label is needed here.
struct ConstructionProgramEntry {
  ConstructionNodeKind node_kind;
  // Body group index (kInstance only; 0 for kGenerate).
  uint32_t body_group;
  // Scope label (relative to parent) offset into path_pool.
  uint32_t label_offset;
  // Parameter data (kInstance only; 0/0 for kGenerate).
  uint32_t param_offset;
  uint32_t param_size;
  // Parent scope index in entry order. UINT32_MAX for root.
  uint32_t parent_scope_index;
  // Ordinal of this node among parent's children.
  uint32_t ordinal_in_parent;
  // For kInstance: compile-time object_index (sorted all_instances position).
  // Used as owner_ordinal so result.instances[object_index] matches the
  // compile-time model. 0 for kGenerate.
  uint32_t instance_index;
  // Realized storage sizes (kInstance only; 0/0 for kGenerate).
  uint64_t realized_inline_size;
  uint64_t realized_appendix_size;
  // Constant port connection initialization: offset and count into the
  // port_const_inits array. Applied immediately after CreateChild returns.
  uint32_t port_const_init_offset;
  uint32_t port_const_init_count;
};

// One constant-valued write to a child instance's port slot.
// Applied inline during child creation in LyraConstructorRunProgram.
struct PortConstInitEntry {
  uint32_t rel_byte_offset;
  uint32_t value_offset;
  uint32_t value_size;
};

// Verify struct layout matches the LLVM emission in
// emit_realization_descriptors.cpp.
static_assert(offsetof(ConstructionProgramEntry, node_kind) == 0);
static_assert(offsetof(ConstructionProgramEntry, body_group) == 4);
static_assert(offsetof(ConstructionProgramEntry, label_offset) == 8);
static_assert(offsetof(ConstructionProgramEntry, param_offset) == 12);
static_assert(offsetof(ConstructionProgramEntry, param_size) == 16);
static_assert(offsetof(ConstructionProgramEntry, parent_scope_index) == 20);
static_assert(offsetof(ConstructionProgramEntry, ordinal_in_parent) == 24);
static_assert(offsetof(ConstructionProgramEntry, instance_index) == 28);
static_assert(offsetof(ConstructionProgramEntry, realized_inline_size) == 32);
static_assert(offsetof(ConstructionProgramEntry, realized_appendix_size) == 40);
static_assert(offsetof(ConstructionProgramEntry, port_const_init_offset) == 48);
static_assert(offsetof(ConstructionProgramEntry, port_const_init_count) == 52);
static_assert(sizeof(ConstructionProgramEntry) == 56);

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
