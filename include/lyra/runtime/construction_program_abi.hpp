#pragma once

#include <cstdint>

#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/engine_types.hpp"

namespace lyra::runtime {

// Construction program entry: one per module instance, in strict ModuleIndex
// order. The runtime replays BeginBody/AddInstance calls from this table.
// Field order chosen to avoid padding (uint64_t first, then uint32_t fields).
struct ConstructionProgramEntry {
  uint64_t storage_base_byte_offset;
  uint32_t body_group;
  uint32_t has_storage;
  uint32_t path_offset;
  uint32_t param_offset;
  uint32_t param_size;
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
  const InitPatchEntry* init_patches;
  uint32_t num_init_patches;
  const InitHandleEntry* init_handles;
  uint32_t num_init_handles;
  const ParamInitSlotEntry* init_param_slots;
  uint32_t num_init_param_slots;
};

}  // namespace lyra::runtime
