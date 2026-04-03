#pragma once

#include <cstdint>

#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

struct BodyDescriptorPackage;
struct RuntimeInstance;

// Per-instance binding record for the R4 structured handoff.
//
// Each bundle connects one module instance to its shared body template.
// The bundle carries only instance-binding facts -- no duplicated
// trigger/comb/slot/process rows. The shared body template
// (BodyDescriptorPackage) remains the single source of template truth.
//
// The body_desc pointer must remain valid for the lifetime of the bundle.
// Constructor stores BodyDescriptorPackage copies in ConstructionResult
// to ensure this.
//
// Constructor produces these. Engine consumes them to derive flat
// runtime registries internally.
struct InstanceMetadataBundle {
  RuntimeInstance* instance = nullptr;
  const BodyDescriptorPackage* body_desc = nullptr;
  // Canonical body identity key for resolving body_desc against final
  // storage after ownership transfer. Set during AddInstance, resolved
  // in Finalize.
  const void* body_key = nullptr;
  InstanceId instance_id = InstanceId{0};
  uint32_t module_proc_base = 0;
  uint32_t num_module_processes = 0;
  const char* instance_path = nullptr;
};

}  // namespace lyra::runtime
