#pragma once

#include <cstdint>
#include <span>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/compiled_module_header.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/port_connection.hpp"

namespace lyra::mir {

// Compile-time identity of a construction program point where a child
// may be created. One site can produce zero, one, or many realized
// descendants at construction time (generate if/for/case).
// Site identity is stable across instances of the same specialization.
struct ChildBindingSiteId {
  uint32_t value = 0;
  auto operator==(const ChildBindingSiteId&) const -> bool = default;
};

// Compile-time child instantiation site within a parent body.
//
// child_spec is part of the pre-body compile contract and must be
// available before parent body recipe compilation begins. This is
// what allows parent-side connection compilation to look up the child's
// CompiledModuleHeader without waiting for the child body to compile.
struct ChildInstantiationSite {
  ChildBindingSiteId site;
  SymbolId instance_sym;
  common::ModuleSpecId child_spec = {};
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
    return {.kind = Kind::kLocalSlot, .local_slot = slot};
  }

  static auto FromFunction(FunctionId fn) -> ConnectionSourceRecipe {
    return {.kind = Kind::kFunction, .function = fn};
  }

  static auto FromExternalRef(ExternalRefId ref) -> ConnectionSourceRecipe {
    return {.kind = Kind::kExternalRef, .external_ref = ref};
  }
};

// Trigger recipe for a connection or process subscription.
//
// Compile-time artifact only. Must not accrete constructor/runtime
// bound state.
struct TriggerRecipe {
  enum class Kind : uint8_t {
    kLocalSlot,
    kExternalRef,
    kFunction,
  };

  Kind kind = Kind::kLocalSlot;
  common::LocalSlotId local_slot;
  ExternalRefId external_ref;
  FunctionId function;
  common::EdgeKind edge = common::EdgeKind::kAnyChange;

  static auto FromLocalSlot(common::LocalSlotId slot, common::EdgeKind e)
      -> TriggerRecipe {
    return {.kind = Kind::kLocalSlot, .local_slot = slot, .edge = e};
  }

  static auto FromExternalRef(ExternalRefId ref, common::EdgeKind e)
      -> TriggerRecipe {
    return {.kind = Kind::kExternalRef, .external_ref = ref, .edge = e};
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

// The ONLY legal parent-side child-port resolution path.
//
// This is a connection-layer consumer of the header boundary, not a
// second owner of header semantics. Header artifact ownership and
// lookup live in compiled_module_header.hpp (HeaderDatabase, FindPort).
//
// Parent-side code must not read child CompiledModuleBody, call
// FindPort directly, or inspect child slot layout through any other
// API. This helper is the structural enforcement of the parent-child
// dependency boundary.
//
// Frozen algorithmic path:
//   1. site -> ChildInstantiationSite.child_spec (from child_sites)
//   2. child_spec -> CompiledModuleHeader (via headers.GetHeader)
//   3. child_port_sym -> PortEntry (via header.FindPort)
//   4. PortEntry -> ChildPortContract
//
// Implemented in B2 when connection compilation is migrated.
auto GetChildPortContract(
    ChildBindingSiteId site, SymbolId child_port_sym,
    const HeaderDatabase& headers,
    std::span<const ChildInstantiationSite> child_sites) -> ChildPortContract;

}  // namespace lyra::mir
