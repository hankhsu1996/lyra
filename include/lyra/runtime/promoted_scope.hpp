#pragma once

#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

// The storage a hold names is a block of members over a definition, and that
// block is built out of the same slots a hold can itself sit in, so the two
// meet here by name rather than by inclusion.
class ManagedObject;

struct ObjectDefinition;

// A hold on the storage a block promoted out of its own frame. LRM 6.21 gives
// a scope enclosing a fork-join block the lifetime of every process that block
// spawned, so the storage outlives the control flow that left the block and
// ends only once nothing names it any more. Taking a copy is what takes a
// hold -- a branch captures one where the `fork` ran -- and the storage ends
// with the last copy, so nothing releases one by hand on any path.
//
// What makes counting the holders exact here is that they are enumerable
// rather than discovered: a program cannot store a reference to an automatic,
// and LRM 9.3.2 bars a detached branch from naming a `ref` formal at all, so
// the only names into this storage are the frame that declared it and the
// branches spawned under it. Both edges run one way in time, so no cycle can
// form and nothing is left for reachability to decide.
class PromotedScopeRef {
 public:
  PromotedScopeRef() = default;

  // The first hold and the storage come into existence together, so there is
  // no moment at which one of them is reachable without the other and no hold
  // ever names storage some other owner ends. The storage comes up through the
  // one allocation every block of its kind comes up through, because what an
  // access reaches a member by is recorded there rather than by the block.
  explicit PromotedScopeRef(const ObjectDefinition* definition);

  // The storage this hold names, which a member of it is reached through.
  [[nodiscard]] auto Storage() const -> ManagedObject*;

 private:
  value::ObjectRef held_;
};

}  // namespace lyra::runtime
