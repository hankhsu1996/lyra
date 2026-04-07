#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/object_index.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/port_connection.hpp"

namespace lyra::mir {

// Bound location: a specific slot on a specific object in the
// construction topology. Shared identity type for external ref
// bindings, connection endpoints, and resolution artifacts.
struct BoundEndpoint {
  common::ObjectIndex object_index;
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
  BoundEndpoint src;
  BoundEndpoint dst;
  BoundEndpoint trigger;
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
  common::ObjectIndex parent_object_index;
  common::ObjectIndex child_object_index;
  common::LocalSlotId child_local_slot;
  TypeId result_type;
  BoundEndpoint trigger;
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

// A fully-bindable ConnectionRecipe resolved against the construction
// topology. All endpoints are concrete (ObjectIndex + LocalSlotId).
// Produced by BindConnectionRecipe for the subset of recipes where
// source and trigger are both kLocalSlot. Recipes with kExternalRef
// or kFunction source/trigger are not represented here.
//
// Produced in design_lower, consumed by the backend to emit kernel
// entries. Parallel to the kernel_bindings subset of ResolvedBindingPlan.
struct BoundConnection {
  uint32_t recipe_index = 0;
  PortConnection::Kind kind = PortConnection::Kind::kDriveParentToChild;
  ModuleBodyId parent_body_id;
  common::ObjectIndex parent_object_index;
  BoundEndpoint child_target;
  BoundEndpoint parent_source;
  BoundEndpoint trigger;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  TypeId result_type;
};

}  // namespace lyra::mir
