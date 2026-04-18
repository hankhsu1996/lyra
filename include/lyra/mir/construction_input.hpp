#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/ext_ref_binding.hpp"
#include "lyra/common/hierarchy_node.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/mir/instance.hpp"

namespace lyra::mir {

// One constant-valued slot initialization within an InstanceConstBlock.
// `body_local_slot` is in body-local slot space (0-based within the
// specialization body).
struct ConstSlotInit {
  uint32_t body_local_slot = 0;
  IntegralConstant value;
};

// Per-object constant initialization data for value-only parameters.
struct InstanceConstBlock {
  std::vector<ConstSlotInit> slot_inits;
};

// Constructor-owned record for one module object in the design.
// body_group is the sole compile-owned lookup key.
struct ObjectRecord {
  uint32_t body_group = 0;
  uint32_t path_index = 0;
  uint64_t realized_inline_size = 0;
  uint64_t realized_appendix_size = 0;
  // Transitional: needed by LLVM backend for design-global layout until B2b
  // deletes the design-global module slot namespace.
  uint32_t design_state_base_slot = 0;
  uint32_t slot_count = 0;
  common::ModuleSpecId spec_id = {};
  // Transitional: consumed only by constructor wiring to resolve port
  // bindings from HIR-level connectivity data. Must not participate in
  // MIR identity, layout, codegen, or runtime slot identity.
  // Removal target: T1-followup (replace with TopologyObjectId).
  SymbolId instance_sym;
};

// Constructor-owned aggregate for all per-object construction data.
// Single source of truth for per-object facts after specialization grouping.
struct ConstructionInput {
  InstanceTable instance_table;
  std::vector<InstanceConstBlock> const_blocks;
  std::vector<ObjectRecord> objects;

  // Per-instance resolved external-ref runtime bindings.
  // Parallel to objects. Each inner vector has one binding per ext-ref recipe
  // in the owning body. Empty for instances whose body has no external refs.
  // Computed by BuildPerInstanceExtRefRuntimeBindings during design lowering.
  std::vector<std::vector<common::SerializedExtRefBinding>>
      instance_ext_ref_bindings;

  // Full scope hierarchy including generate scopes. Strict construction
  // order: parents before children. Module instances and generate scopes
  // interleaved. Built at AST->HIR time, threaded through MIR lowering.
  // Consumed by construction program emitter to produce scope-aware
  // runtime hierarchy.
  std::vector<common::HierarchyNode> hierarchy_nodes;
};

}  // namespace lyra::mir
