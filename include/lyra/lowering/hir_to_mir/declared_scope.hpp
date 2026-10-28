#pragma once

#include <map>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/registry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/fixed_name.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// SV gives a procedural body a tree of lexical scopes -- a task or function
// body, a `begin ... end`, a `fork ... join` (LRM 23.9). Two things about a
// scope outlive the activation that opened it: the place a hierarchical name
// reaches it at (LRM 23.6), and what a `disable` naming it invalidates (LRM
// 9.6.2). A static-lifetime variable declared in it also outlives every
// activation (LRM 6.21) -- but that is storage of the declaration like any
// other, placed by the rule that places every static cell.
//
// So a scope has no run-time existence of its own: what a body there keeps is a
// field of the class enclosing it, one member access from that body's `self`,
// and the place a hierarchical name reaches it at is a member of what the
// enclosing scope publishes. For
//
//   initial begin : outer
//     static int x;
//     begin : inner ... end
//   end
//
// the enclosing class holds the cell `x` and the target a `disable outer`
// ends, and `x` is published under the name the source wrote, since a
// hierarchical name reaches it (LRM 23.6).
//
// How the scopes nest is the HIR scope tree, and nothing here restates it.
// What this states is only what each scope got.

// An event a scope's beginning or end triggers for a covergroup sampled there
// (LRM 19.3): the one kept for the covergroup's type, where `home` keeps it,
// or, in a method of a class embedding the covergroup, the event of the record
// of the instance the object's property `holder` holds, where it holds one.
struct TypeEventCell {
  StaticStorageHome home;
};

struct HeldCovergroupEvent {
  mir::ClassFieldTarget holder;
  mir::TypeId handle_type;
  hir::CovergroupId covergroup;
};

using BlockEventTrigger = std::variant<TypeEventCell, HeldCovergroupEvent>;

// The events one scope triggers as it is entered and as it is left.
struct ScopeBlockEvents {
  std::vector<BlockEventTrigger> at_begin;
  std::vector<BlockEventTrigger> at_end;
};

// The events each scope of one declaration scope triggers, for the scopes some
// covergroup is sampled at.
using BlockEventTriggers = std::map<hir::ProceduralScopeId, ScopeBlockEvents>;

// What one procedural scope was given: the name a construct inside it reports
// for itself (LRM 21.2.1.5), and the cell a `disable` naming the scope
// invalidates (LRM 9.6.2). The target belongs to a scope the source named and
// to no other, since what a `disable` reaches is what a name reaches -- so
// nothing has to first find out which scopes some `disable` names. It is owned
// wherever that scope's static-lifetime state is owned: an instance, a class
// object, a package.
//
// A scope a covergroup is sampled at the beginning or end of also triggers an
// event as it is entered and as it is left by its end or by a `return`,
// `break` or `continue` out of it, but not by a `disable` (LRM 19.3, 9.6.2).
struct DeclaredScope {
  hir::FixedName name;
  std::optional<StaticStorageHome> disable_target;
  ScopeBlockEvents block_events = {};
};

// What each procedural scope of one declaration scope was given, reached by
// that scope's own id.
using DeclaredScopes = base::Translation<hir::ProceduralScopeId, DeclaredScope>;

// The events `triggers` names for the scope `id`, and none where it names none.
[[nodiscard]] inline auto BlockEventsOf(
    const BlockEventTriggers& triggers, hir::ProceduralScopeId id)
    -> ScopeBlockEvents {
  const auto found = triggers.find(id);
  return found == triggers.end() ? ScopeBlockEvents{} : found->second;
}

// The name each procedural scope of a declaration scope reports, by the
// scope's own id: the declaration scope's name `declaring`, joined by `::` to
// the root scope of each body where the source named it, then each scope's
// joined by `.` to the scope around it. A scope the source gave no name
// reports the name of the one around it.
[[nodiscard]] auto ReportedNames(
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    const hir::FixedName& declaring)
    -> base::Translation<hir::ProceduralScopeId, hir::FixedName>;

// The finished answer for a declaration scope outside the design hierarchy,
// whose name is `declaring`: a scope the source named owns the target a
// `disable` invalidates, on `owner`.
// Every scope still has an entry, so a body there reads its answer the same way
// a body anywhere else does.
[[nodiscard]] inline auto ScopesOwningDisableTargets(
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    const hir::FixedName& declaring, const StaticStorageOwner& owner,
    mir::TypeId target_type, const BlockEventTriggers& triggers = {})
    -> DeclaredScopes {
  const base::Translation<hir::ProceduralScopeId, hir::FixedName> names =
      ReportedNames(scopes, declaring);
  std::vector<DeclaredScope> declared;
  declared.reserve(scopes.size());
  for (const hir::ProceduralScopeId id : scopes.Ids()) {
    const hir::ProceduralScopeDecl& scope = scopes.Get(id);
    std::optional<StaticStorageHome> target;
    if (scope.source_name.has_value()) {
      target = DeclareStaticCell(owner, target_type);
    }
    declared.push_back(
        DeclaredScope{
            .name = names.Get(id),
            .disable_target = std::move(target),
            .block_events = BlockEventsOf(triggers, id)});
  }
  return {scopes.size(), std::move(declared)};
}

}  // namespace lyra::lowering::hir_to_mir
