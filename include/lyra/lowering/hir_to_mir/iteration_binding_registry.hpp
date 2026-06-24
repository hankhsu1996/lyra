#pragma once

#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/with_clause_id.hpp"
#include "lyra/lowering/hir_to_mir/block_depth.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::lowering::hir_to_mir {

// One synthesized closure parameter for an array-method `with` clause (LRM
// 7.12.4): the body slot it lives in and the depth it was declared at, which is
// all the capture decision needs (a reference from a deeper clause's closure
// captures it; one at the same depth reads it directly).
struct IterationBinding {
  mir::LocalId var;
  BlockDepth decl_depth;
};

// Maps each active `with` clause's identity to its element and index closure
// parameters. A reference resolves purely by clause identity and role, so a
// clause nested in another's body still reaches the outer parameter -- the
// registry is shared down the nesting, every enclosing clause stays reachable.
// Entries are appended as closures are built and never collide (clause ids are
// unique within the unit).
class IterationBindingRegistry {
 public:
  void Register(
      hir::WithClauseId clause, IterationBinding element,
      IterationBinding index) {
    clauses_.push_back({clause, {.element = element, .index = index}});
  }

  [[nodiscard]] auto Lookup(
      hir::WithClauseId clause, hir::IterationBindingRole role) const
      -> IterationBinding {
    for (const auto& [id, bindings] : clauses_) {
      if (id == clause) {
        return role == hir::IterationBindingRole::kElement ? bindings.element
                                                           : bindings.index;
      }
    }
    throw InternalError(
        "IterationBindingRegistry::Lookup: with-clause iteration reference "
        "names an unregistered clause");
  }

 private:
  struct ClauseBindings {
    IterationBinding element;
    IterationBinding index;
  };

  std::vector<std::pair<hir::WithClauseId, ClauseBindings>> clauses_;
};

}  // namespace lyra::lowering::hir_to_mir
