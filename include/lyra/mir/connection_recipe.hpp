#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/symbol_types.hpp"
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

// Connection source execution category:
//   kLocalSlot -- parent-body-local slot (direct copy, memcpy c2p path)
//   kFunction  -- body-local compiled writeback function (p2c path)
//   kConstant  -- compile-time constant (p2c constant-init path)
//
// Compile-time artifact only. Must not accrete constructor/runtime
// bound state. The separation from construction execution is hard.
struct ConnectionSourceRecipe {
  enum class Kind : uint8_t {
    kLocalSlot,
    kFunction,
    kConstant,
  };

  Kind kind = Kind::kLocalSlot;
  common::LocalSlotId local_slot;
  FunctionId function;
  ConstId constant_id;

  static auto FromLocalSlot(common::LocalSlotId slot)
      -> ConnectionSourceRecipe {
    return {
        .kind = Kind::kLocalSlot,
        .local_slot = slot,
        .function = {},
        .constant_id = {},
    };
  }

  static auto FromFunction(FunctionId fn) -> ConnectionSourceRecipe {
    return {
        .kind = Kind::kFunction,
        .local_slot = {},
        .function = fn,
        .constant_id = {},
    };
  }

  static auto FromConstant(ConstId id) -> ConnectionSourceRecipe {
    return {
        .kind = Kind::kConstant,
        .local_slot = {},
        .function = {},
        .constant_id = id,
    };
  }
};

// Trigger recipe for a memcpy-path c2p connection.
//
// Consumed only by the kDriveChildToParent memcpy pipeline
// (IsFullyBindableRecipe, BindConnectionRecipe). kDriveParentToChild
// recipes carry a default-initialized TriggerRecipe{} that is never
// read; narrowing the struct to split p2c and c2p shapes is a candidate
// future cleanup.
//
// Compile-time artifact only. Must not accrete constructor/runtime
// bound state.
struct TriggerRecipe {
  enum class Kind : uint8_t {
    // Parent body-local slot. Trigger fires on this parent slot's change.
    kLocalSlot,
    // Child body-local slot. Trigger fires on this child slot's change.
    // Used by kDriveChildToParent connections where the trigger is the
    // child's output port, which is child-local, not parent-local.
    kChildSlot,
  };

  Kind kind = Kind::kLocalSlot;
  common::LocalSlotId local_slot;
  common::EdgeKind edge = common::EdgeKind::kAnyChange;

  static auto FromLocalSlot(common::LocalSlotId slot, common::EdgeKind e)
      -> TriggerRecipe {
    return {.kind = Kind::kLocalSlot, .local_slot = slot, .edge = e};
  }

  static auto FromChildSlot(common::LocalSlotId slot, common::EdgeKind e)
      -> TriggerRecipe {
    return {.kind = Kind::kChildSlot, .local_slot = slot, .edge = e};
  }
};

// Body-local connection recipe.
//
// Shared struct for both directions. Some fields are direction-scoped:
//   kDriveParentToChild consumes: source (kFunction), target_ref.
//     child_slot and trigger are default-initialized and never read.
//   kDriveChildToParent consumes: source (kLocalSlot), trigger,
//     child_slot. target_ref is unused.
//
// child_slot (kDriveChildToParent only) is the child specialization's
// lowered LocalSlotId obtained exclusively through GetChildPortContract
// (which reads the child specialization's CompiledModuleHeader). It is
// NOT a source-level declaration ordinal and must NOT be computed from
// source port order or child body declaration tables.
//
// Compile-time artifact only. Must not accrete constructor/runtime
// bound state. The separation from construction execution is hard.
struct ConnectionRecipe {
  PortConnection::Kind kind = PortConnection::Kind::kDriveParentToChild;
  ChildBindingSiteId child_site;
  // kDriveChildToParent only: child body-local slot for the memcpy
  // pipeline. Default-initialized and unused on kDriveParentToChild.
  common::LocalSlotId child_slot;
  // kDriveParentToChild only: external-ref id identifying the child
  // target. Registered in the parent body's external_refs at recipe
  // creation; consumed by the writeback body's assignment statement and
  // by the installable-computation template.
  ExternalRefId target_ref;
  ConnectionSourceRecipe source;
  // kDriveChildToParent only. Default-initialized on kDriveParentToChild
  // and never read.
  TriggerRecipe trigger;
  common::OriginId origin = common::OriginId::Invalid();
};

// Body-local template for an installable computation.
// Created by BuildInstallableComputations during MIR construction.
//
// An installable computation is an ordinary reactive writeback body:
//   - eval: body-local MIR function compiled via DefineMirFunction
//   - target: reached from inside the callable through the
//     WriteTarget{ExternalRefId} statement emitted at recipe creation
//     time, routed through the parent body's ordinary external_refs
//     table and per-instance ext_ref_bindings at runtime
//   - deps: parent-local slots the callable reads
//
// One template per reactive parent->child port binding. Runtime uses
// this to schedule initial evaluation and register dependency
// triggers; everything else (child target, value type, write ABI) is
// encoded in the MIR body and the external-ref binding system.
//
// Not a process. Not scheduled. Initial-evaluated once during
// construction, then re-evaluated by fixpoint dispatch when a
// dependency slot changes.
struct InstallableComputationTemplate {
  // Body-local writeback function (module-scoped). Void-returning body
  // that writes its child target through an ExternalRefId-backed
  // assignment; no return-value transport.
  FunctionId callable;
  // Parent-local dependency slots. Empty for constant expressions.
  std::vector<common::LocalSlotId> deps;
};

}  // namespace lyra::mir
