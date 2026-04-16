#pragma once

#include <cstdint>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/child_binding_site_id.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/port_connection.hpp"

namespace lyra::mir {

// Compile-time child instantiation site within a parent body.
//
// child_spec is part of the pre-body compile contract and must be
// available before parent body recipe compilation begins. This is
// what allows parent-side connection compilation to look up the child's
// CompiledModuleHeader without waiting for the child body to compile.
//
// id is the one canonical durable identity for this child, sufficient
// for construction-time binding. It is (coord, child_ordinal) from the
// parent's repertoire descriptor. All binding/resolution uses id.
//
// ChildBindingSiteId is a separate compile-time indexing aid for
// body-local bookkeeping (indexing into child_sites vectors). It is
// NOT the identity used by runtime/construction binding.
struct ChildInstantiationSite {
  ChildBindingSiteId site;
  DurableChildId id;
  common::ModuleSpecId child_spec = {};
  // Retained for diagnostics/debugging only. Not used for binding.
  SymbolId debug_instance_sym;
  common::OriginId origin = common::OriginId::Invalid();
};

// Connection source classification. Exactly three kinds:
//   kLocalSlot   -- parent-body-local slot (direct copy)
//   kFunction    -- body-local compiled function (expression)
//   kExternalRef -- non-local external reference handle
//
// These are the only legal parent-side source categories for connection
// recipes. No ad hoc intermediate categories or topology-dependent
// classifications may be added. Source classification is semantic and
// body-local, never topology-dependent.
//
// Compile-time artifact only. Must not accrete constructor/runtime
// bound state. The separation from construction execution is hard.
struct ConnectionSourceRecipe {
  enum class Kind : uint8_t {
    kLocalSlot,
    kFunction,
    kExternalRef,
  };

  Kind kind = Kind::kLocalSlot;
  common::LocalSlotId local_slot;
  FunctionId function;
  ExternalRefId external_ref;

  static auto FromLocalSlot(common::LocalSlotId slot)
      -> ConnectionSourceRecipe {
    return {
        .kind = Kind::kLocalSlot,
        .local_slot = slot,
        .function = {},
        .external_ref = {}};
  }

  static auto FromFunction(FunctionId fn) -> ConnectionSourceRecipe {
    return {
        .kind = Kind::kFunction,
        .local_slot = {},
        .function = fn,
        .external_ref = {}};
  }

  static auto FromExternalRef(ExternalRefId ref) -> ConnectionSourceRecipe {
    return {
        .kind = Kind::kExternalRef,
        .local_slot = {},
        .function = {},
        .external_ref = ref};
  }
};

// Trigger recipe for a connection or process subscription.
//
// Compile-time artifact only. Must not accrete constructor/runtime
// bound state.
struct TriggerRecipe {
  enum class Kind : uint8_t {
    // Parent body-local slot. Trigger fires on this parent slot's change.
    kLocalSlot,
    kExternalRef,
    kFunction,
    // Child body-local slot. Trigger fires on this child slot's change.
    // Used by kDriveChildToParent connections where the trigger is the
    // child's output port, which is child-local, not parent-local.
    kChildSlot,
  };

  Kind kind = Kind::kLocalSlot;
  common::LocalSlotId local_slot;
  ExternalRefId external_ref;
  FunctionId function;
  common::EdgeKind edge = common::EdgeKind::kAnyChange;

  static auto FromLocalSlot(common::LocalSlotId slot, common::EdgeKind e)
      -> TriggerRecipe {
    return {
        .kind = Kind::kLocalSlot,
        .local_slot = slot,
        .external_ref = {},
        .function = {},
        .edge = e};
  }

  static auto FromChildSlot(common::LocalSlotId slot, common::EdgeKind e)
      -> TriggerRecipe {
    return {
        .kind = Kind::kChildSlot,
        .local_slot = slot,
        .external_ref = {},
        .function = {},
        .edge = e};
  }

  static auto FromExternalRef(ExternalRefId ref, common::EdgeKind e)
      -> TriggerRecipe {
    return {
        .kind = Kind::kExternalRef,
        .local_slot = {},
        .external_ref = ref,
        .function = {},
        .edge = e};
  }
};

// Body-local connection recipe.
//
// child_slot is the child specialization's lowered LocalSlotId obtained
// exclusively through GetChildPortContract (which reads the child
// specialization's CompiledModuleHeader). It is NOT a source-level
// declaration ordinal and must NOT be computed from source port order
// or child body declaration tables.
//
// Compile-time artifact only. Must not accrete constructor/runtime
// bound state. The separation from construction execution is hard.
struct ConnectionRecipe {
  PortConnection::Kind kind = PortConnection::Kind::kDriveParentToChild;
  ChildBindingSiteId child_site;
  common::LocalSlotId child_slot;
  ConnectionSourceRecipe source;
  TriggerRecipe trigger;
  TypeId result_type;
  common::OriginId origin = common::OriginId::Invalid();
};

// Body-local template for an expression connection process.
// Created by LowerExprConnectionForBody during MIR construction.
// Contains no whole-design object identity. Parallel to the expression
// connection suffix of body.processes:
//   body.processes[body.processes.size() - expr_connection_templates.size() +
//   i] corresponds to expr_connection_templates[i].
struct ExprConnectionTemplate {
  // Ordinal within the expression connection suffix (0..N-1).
  // Actual body.processes index = num_ordinary + expr_process_suffix_ordinal.
  uint32_t expr_process_suffix_ordinal = 0;
  // Child destination (body-local).
  ChildBindingSiteId child_site;
  common::LocalSlotId child_slot;
  // Value type.
  TypeId result_type;
};

}  // namespace lyra::mir
