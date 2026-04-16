#pragma once

#include <cstdint>
#include <format>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/compiled_module_header.hpp"
#include "lyra/mir/connection_recipe.hpp"
#include "lyra/mir/cover_site.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

enum class SlotKind : uint8_t;
struct SlotDesc;
struct EventDesc;

// Implementation-detail body container, pending migration to
// CompiledModuleBody (compiled_specialization.hpp) in B2.
//
// CompiledModuleBody is the ONLY public specialization-scoped body
// artifact type going forward. New code must not depend on this type
// as a public contract. Existing consumers will be migrated in B2.
//
// Owns all body-local MIR storage: processes, functions, places, and
// slot descriptors. ProcessIds, FunctionIds, and body-local PlaceIds are
// indices into the embedded arena. They are permanently body-local.
//
// Invariants:
// - No instance identity (no instance_sym, no instance path strings)
// - No placement/layout ownership (no slot offsets)
// - No assembly-scoped data (no binding/connectivity metadata)
// - No design-global routing metadata
// - All body-local IDs resolve through arena (no rebasing)
struct ModuleBody {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;

  // Body-local slot descriptors, indexed by kModuleSlot id.
  // This is the body's required storage interface: what slots exist,
  // their kinds, and their types. This is NOT placement/layout metadata.
  std::vector<SlotDesc> slots;

  // Body-local trace names, parallel to slots (indexed by kModuleSlot id).
  // Source: sym.name from CollectBodyLocalDecls -> BodyLocalDecls -> here.
  // Invariant: local_trace_names.size() == slots.size().
  std::vector<std::string> local_trace_names;

  // Body-local named event descriptors. Indexed by body-local EventId.
  std::vector<EventDesc> events;

  // Per-body timescale from source-level scope metadata.
  // Set during HIR-to-MIR lowering from body_timescales.
  int8_t time_unit_power = 0;
  int8_t time_precision_power = 0;

  // Total body-global decision site count. Set by module lowering after all
  // functions, processes, and tasks are lowered. Defines the authoritative
  // size of the body-wide decision metadata table. The LLVM backend validates
  // that exactly this many sites are reconstructed from the stored records.
  uint32_t total_decision_sites = 0;

  // Body-local MIR storage. All body-local PlaceIds, ProcessIds, and
  // FunctionIds are indices into this arena.
  Arena arena;

  // B2: External access recipes for this body.
  // Specialization-scoped: contains only instance-invariant data (upward_count,
  // canonical durable child path, target_local_slot, type, access_kind).
  // No concrete target_object. Per-instance resolution happens at construction
  // time via ConstructionInput::instance_ext_ref_bindings.
  std::vector<ExternalAccessRecipe> external_refs;

  // Body-local public port entries. Populated from CompiledModuleHeader
  // during design lowering. Emitted to runtime for constructor-side
  // symbolic port -> local slot resolution.
  // Uses the canonical mir::PortEntry from compiled_module_header.hpp.
  std::vector<mir::PortEntry> port_entries;

  // B2: Child instantiation sites for this body.
  std::vector<ChildInstantiationSite> child_sites;

  // B2: Connection recipes for this body.
  std::vector<ConnectionRecipe> connection_recipes;

  // Expression connection process templates. Parallel metadata for the
  // suffix of body.processes that are expression connections.
  // Use GetExprConnectionCount / GetOrdinaryProcessCount / etc. for indexing.
  // Created only by LowerExprConnectionForBody. No other code may mutate.
  std::vector<ExprConnectionTemplate> expr_connection_templates;

  // Body-owned cover sites. CoverSiteId in MIR effects is body-local
  // (0-based per body). The design-global table is built by concatenation
  // at assembly time.
  std::vector<ImmediateCoverSiteInfo> immediate_cover_sites;

  // Body-owned deferred assertion sites. DeferredAssertionSiteId in MIR
  // effects is body-local (0-based per body). The design-global table is
  // built by concatenation at assembly time.
  std::vector<DeferredAssertionSiteInfo> deferred_assertion_sites;
};

// Canonical suffix/ordinal helpers for the expression connection process
// suffix on ModuleBody. Every consumer of the suffix invariant must use
// these helpers instead of open-coded arithmetic. Helpers fail fast on
// invariant violation.

[[nodiscard]] inline auto GetExprConnectionCount(const ModuleBody& body)
    -> uint32_t {
  return static_cast<uint32_t>(body.expr_connection_templates.size());
}

[[nodiscard]] inline auto GetOrdinaryProcessCount(const ModuleBody& body)
    -> uint32_t {
  const auto total = static_cast<uint32_t>(body.processes.size());
  const auto expr = GetExprConnectionCount(body);
  if (expr > total) {
    throw common::InternalError(
        "GetOrdinaryProcessCount",
        std::format("expr_count {} > total_processes {}", expr, total));
  }
  return total - expr;
}

[[nodiscard]] inline auto GetExprConnectionProcessOrdinal(
    const ModuleBody& body, uint32_t suffix_ordinal) -> uint32_t {
  const auto expr = GetExprConnectionCount(body);
  if (suffix_ordinal >= expr) {
    throw common::InternalError(
        "GetExprConnectionProcessOrdinal",
        std::format(
            "suffix_ordinal {} >= expr_count {}", suffix_ordinal, expr));
  }
  return GetOrdinaryProcessCount(body) + suffix_ordinal;
}

[[nodiscard]] inline auto IsExprConnectionProcessOrdinal(
    const ModuleBody& body, uint32_t ordinal) -> bool {
  return ordinal >= GetOrdinaryProcessCount(body) &&
         ordinal < static_cast<uint32_t>(body.processes.size());
}

}  // namespace lyra::mir
