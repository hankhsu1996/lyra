#pragma once

#include <cstddef>
#include <optional>
#include <vector>

#include "lyra/base/translation.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::lowering::hir_to_mir {

// SV gives a procedural body a tree of lexical scopes -- a task or function
// body, a `begin ... end`, a `fork ... join` (LRM 23.9). Two things about a
// scope outlive the activation that opened it: the name a construct inside it
// reports for itself (LRM 21.2.1.5), and what a `disable` naming it invalidates
// (LRM 9.6.2). A static-lifetime local declared in it also outlives every
// activation (LRM 13.3.1) -- but that is per-instance storage like any other
// and sits on the instance; what belongs to the scope is only the spelling a
// hierarchical path reaches it by.
//
// So a scope becomes a name node, a runtime object carrying the identity a
// hierarchical path matches, while everything it owns storage-wise is a field
// of the class enclosing the body. Both are one member access from that body's
// `self`:
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
// A scope owns neither in a declaration scope that is not part of the design
// hierarchy. A class object is reached by member select rather than by scope
// name (LRM 23.7) and a package owns no object at all (LRM 26.3), so nothing
// there answers for a name and no `disable` reaches in; a body there reports
// the enclosing object's own name.
struct DeclaredScope {
  std::optional<ScopeNameNode> name_node;
  std::optional<mir::FieldId> cancellation_target;

  // The handle a body reads off its `self` to reach this scope's name node.
  [[nodiscard]] auto NameBorrowedHandle() const -> std::optional<mir::FieldId> {
    return name_node.transform(
        [](const ScopeNameNode& node) { return node.borrowed_handle; });
  }
};

// What each procedural scope of one declaration scope was given, reached by
// that scope's own id.
using DeclaredScopes = base::Translation<hir::ProceduralScopeId, DeclaredScope>;

// The finished answer for a declaration scope outside the design hierarchy,
// where no scope owns anything. Every scope still has an entry, so a body there
// reads its answer the same way a body anywhere else does.
[[nodiscard]] inline auto ScopesOwningNothing(std::size_t scope_count)
    -> DeclaredScopes {
  return {scope_count, std::vector<DeclaredScope>(scope_count)};
}

}  // namespace lyra::lowering::hir_to_mir
