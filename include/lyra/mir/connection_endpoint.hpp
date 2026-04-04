#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/port_connection.hpp"

namespace lyra::mir {

// Topology-owned connection endpoint: identifies a specific slot on a
// specific object. Shared identity type for both kernelized and
// non-kernelized connection paths.
struct ConnectionEndpointRef {
  uint32_t object_index = 0;
  common::LocalSlotId local_slot;
};

// Forward declaration for trigger observation metadata.
struct ResolvedObservation {
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
  uint8_t bit_index = 0;
};

// Kernelized connection: simple assign with one trigger.
// No MIR process body needed. Source value is directly copied
// from src endpoint to dst endpoint on trigger.
struct ResolvedKernelBinding {
  PortConnection::Kind kind = PortConnection::Kind::kDriveParentToChild;
  ConnectionEndpointRef src;
  ConnectionEndpointRef dst;
  ConnectionEndpointRef trigger;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  std::optional<ResolvedObservation> trigger_observation;
  TypeId value_type;
  SymbolId child_port_sym;
  SymbolId parent_instance_sym;
};

// Non-kernelized connection: parent-body-local expression evaluation
// plus explicit child endpoint delivery.
// expr_function is a normal body-local callable stored in the parent
// body's function list / arena. It reads parent values through kModuleSlot.
// At execution/codegen time, receives parent this_ptr. Its return value
// is written to the child endpoint via explicit topology routing.
struct CompiledConnectionExpr {
  PortConnection::Kind kind = PortConnection::Kind::kDriveParentToChild;
  ModuleBodyId parent_body_id;
  FunctionId expr_function;
  uint32_t parent_object_index = 0;
  uint32_t child_object_index = 0;
  common::LocalSlotId child_local_slot;
  TypeId result_type;
  ConnectionEndpointRef trigger;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  SymbolId child_port_sym;
  SymbolId parent_instance_sym;
};

// Replaces CompiledBindingPlan. Contains both kernelized and
// non-kernelized connection artifacts using shared endpoint identity.
struct ResolvedBindingPlan {
  std::vector<ResolvedKernelBinding> kernel_bindings;
  std::vector<CompiledConnectionExpr> expr_bindings;
};

}  // namespace lyra::mir
