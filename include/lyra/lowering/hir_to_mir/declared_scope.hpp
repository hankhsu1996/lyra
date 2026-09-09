#pragma once

#include <format>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/registry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// SV gives a procedural body a tree of lexical scopes -- a task or function
// body, a `begin ... end`, a `fork ... join` (LRM 23.9). Two things about a
// scope outlive the activation that opened it: the name a construct inside it
// reports for itself (LRM 21.2.1.5), and what a `disable` naming it invalidates
// (LRM 9.6.2). A static-lifetime local declared in it also outlives every
// activation (LRM 6.21) -- but that is storage of the declaration like any
// other, placed by the rule that places every static cell; what belongs to the
// scope is only the spelling a hierarchical path reaches it by.
//
// So a scope of the design hierarchy becomes a name node, a runtime object
// carrying the identity a hierarchical path matches, and everything a body
// there keeps is a field of the class enclosing it -- both one member access
// from that body's `self`:
//
//   initial begin : outer        self->outer_borrowed_handle  reports the name
//     static int x;              self->outer__x               holds the value
//     begin : inner ... end      self->inner_borrowed_handle
//
// How the nodes nest is the HIR scope tree, and nothing here restates it. What
// this states is only what each scope got.

// The name node one procedural scope owns: the class its runtime object is an
// instance of, and the borrowed handle the enclosing class keeps on it. The
// handle is typed to the node's class like every other owned child's, which is
// what keeps a class's layout a complete statement of which objects the runtime
// builds under it.
struct ScopeNameNode {
  mir::ClassId class_id{};
  mir::FieldId borrowed_handle{};
};

// What one procedural scope owns at run time: a name node, and the cell a
// `disable` naming the scope invalidates (LRM 9.6.2). The second belongs to a
// scope the source named and to no other, since what a `disable` reaches is
// what a name reaches -- so nothing has to first find out which scopes some
// `disable` names.
//
// The two are not owned together. A name node answers for a hierarchical path,
// which only the design hierarchy has: a class object is reached by member
// select rather than by scope name (LRM 23.7) and a package owns no object at
// all (LRM 26.3), so a body there reports the enclosing object's own name and
// owns no node. A `disable` written inside such a body still names its own
// block or task, so the target is owned wherever that scope's static-lifetime
// state is owned.
struct DeclaredScope {
  std::optional<ScopeNameNode> name_node;
  std::optional<StaticStorageHome> cancellation_target;

  // The handle a body reads off its `self` to reach this scope's name node.
  [[nodiscard]] auto NameBorrowedHandle() const -> std::optional<mir::FieldId> {
    return name_node.transform(
        [](const ScopeNameNode& node) { return node.borrowed_handle; });
  }
};

// What each procedural scope of one declaration scope was given, reached by
// that scope's own id.
using DeclaredScopes = base::Translation<hir::ProceduralScopeId, DeclaredScope>;

// The finished answer for a declaration scope outside the design hierarchy: no
// scope answers for a name there, and a scope the source named owns the target
// a `disable` invalidates. Every scope still has an entry, so a body there
// reads its answer the same way a body anywhere else does.
[[nodiscard]] inline auto ScopesOwningDisableTargets(
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    const StaticStorageOwner& owner, mir::TypeId target_type)
    -> DeclaredScopes {
  std::vector<DeclaredScope> declared;
  declared.reserve(scopes.size());
  for (const hir::ProceduralScopeId id : scopes.Ids()) {
    const hir::ProceduralScopeDecl& scope = scopes.Get(id);
    std::optional<StaticStorageHome> target;
    if (scope.source_name.has_value()) {
      target = DeclareStaticCell(
          owner, std::format("{}__cancel_{}", *scope.source_name, id.value),
          target_type);
    }
    declared.push_back(
        DeclaredScope{
            .name_node = std::nullopt,
            .cancellation_target = std::move(target)});
  }
  return {scopes.size(), std::move(declared)};
}

}  // namespace lyra::lowering::hir_to_mir
