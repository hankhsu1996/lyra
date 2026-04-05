#pragma once

#include <vector>

#include "lyra/common/module_identity.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/compiled_module_header.hpp"
#include "lyra/mir/connection_recipe.hpp"
#include "lyra/mir/design.hpp"  // for SlotDesc (extraction to own header is a B2 item)
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// Specialization-scoped compiled body artifact.
//
// This is the ONLY public specialization-scoped body artifact type going
// forward. Existing mir::ModuleBody is an implementation detail pending
// migration in B2; new code must not depend on it as a public contract.
//
// May contain only specialization-scoped templates and recipes.
// No per-instance resolved bindings, realized descendant lists,
// or final topology-derived metadata may be stored here.
struct CompiledModuleBody {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
  std::vector<SlotDesc> slots;
  Arena arena;

  // Recipe artifacts (specialization-scoped templates only)
  std::vector<ExternalAccessRecipe> external_refs;
  std::vector<ChildInstantiationSite> child_sites;
  std::vector<ConnectionRecipe> connection_recipes;
  // construction_ops deferred to B2/B3 -- shape depends on
  // NonLocalTargetRecipe which is not yet frozen.
};

// Header and body are siblings, not nested.
//
// Three compile-time artifact layers:
//   CompiledModuleHeader  -- specialization-scoped lowered child contract
//   CompiledModuleBody    -- specialization-scoped body artifact
//   CompiledSpecialization -- container holding header + body as siblings
//
// The header is a separately owned artifact in a HeaderDatabase, referenced
// by stable handle (CompiledModuleHeaderId). Not nested ownership.
//
// Invariant: header_id must resolve to a header whose spec_id matches
// this spec_id. Enforced by the factory function MakeCompiledSpecialization.
//
// Construction must go through MakeCompiledSpecialization. Direct
// aggregate construction is not the public API.
class CompiledSpecialization {
 public:
  [[nodiscard]] auto SpecId() const -> common::ModuleSpecId {
    return spec_id_;
  }
  [[nodiscard]] auto HeaderId() const -> CompiledModuleHeaderId {
    return header_id_;
  }
  [[nodiscard]] auto Body() -> CompiledModuleBody& {
    return body_;
  }
  [[nodiscard]] auto Body() const -> const CompiledModuleBody& {
    return body_;
  }

 private:
  friend auto MakeCompiledSpecialization(
      common::ModuleSpecId spec_id, CompiledModuleHeaderId header_id,
      const HeaderDatabase& headers, CompiledModuleBody body)
      -> CompiledSpecialization;

  common::ModuleSpecId spec_id_ = {};
  CompiledModuleHeaderId header_id_;
  CompiledModuleBody body_;
};

// Factory that enforces the spec_id <-> header_id consistency invariant.
// Throws InternalError if the header's spec_id does not match.
auto MakeCompiledSpecialization(
    common::ModuleSpecId spec_id, CompiledModuleHeaderId header_id,
    const HeaderDatabase& headers, CompiledModuleBody body)
    -> CompiledSpecialization;

}  // namespace lyra::mir
