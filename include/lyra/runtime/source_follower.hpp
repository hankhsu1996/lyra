#pragma once

#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

// What keeps storage of its own showing the place it stands for, while a
// procedural continuous assignment covers part of it (LRM 10.6.2): a port
// standing for what its connection drives, or a net showing the object its one
// driver carries, is otherwise that place itself, with nothing running to
// carry it (LRM 23.3.3). It is told of every change the place sees and runs
// `changed` for each, at once, as the place itself would have shown it, for as
// long as it lives; it wakes nothing, and ending it ends what it watches.
class SourceFollower {
 public:
  SourceFollower(const Trigger& source, OwnedCall changed);
  SourceFollower(const SourceFollower&) = delete;
  auto operator=(const SourceFollower&) -> SourceFollower& = delete;
  // Non-movable: the place it is enrolled on points at its membership.
  SourceFollower(SourceFollower&&) = delete;
  auto operator=(SourceFollower&&) -> SourceFollower& = delete;
  ~SourceFollower();

  // A change reached the place.
  void Reached();

 private:
  OwnedCall changed_;
  WaitMembership membership_;
};

}  // namespace lyra::runtime
