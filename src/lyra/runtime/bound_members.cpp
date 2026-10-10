#include "lyra/runtime/bound_members.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/support/takeover_level.hpp"

namespace lyra::runtime {

BoundMembers::BoundMembers() = default;
BoundMembers::~BoundMembers() = default;

void BoundMembers::Bind(ErasedReference& member, const ErasedReference& bound) {
  member.member = &member;
  // A reference that names nothing yet is itself a member whose own binding
  // has not run: the order scopes bind in is the hierarchy's, and a connection
  // may name a member of a scope bound later. What is bound from it follows
  // when it is.
  const bool bound_later = bound.storage == nullptr;
  if (const ErasedReference* from = bound_later ? &bound : bound.member) {
    members_[from].bound_from_it.push_back(&member);
  }
  if (bound_later) {
    return;
  }
  Move(Following(member), nullptr, bound);
}

auto BoundMembers::Forced(const ErasedReference& member) const -> bool {
  const auto at = members_.find(&member);
  return at != members_.end() && at->second.driver != nullptr;
}

auto BoundMembers::NextGeneration(ErasedReference& member) -> std::int64_t {
  return ++members_[&member].generation;
}

auto BoundMembers::Following(ErasedReference& member) const -> Followers {
  Followers followers{.naming = {&member}, .forced_below = {}};
  for (std::size_t next = 0; next < followers.naming.size(); ++next) {
    const auto at = members_.find(followers.naming[next]);
    if (at == members_.end()) continue;
    for (ErasedReference* bound : at->second.bound_from_it) {
      const auto below = members_.find(bound);
      if (below != members_.end() && below->second.driver != nullptr) {
        followers.forced_below.push_back(below->second.driver.get());
      } else {
        followers.naming.push_back(bound);
      }
    }
  }
  return followers;
}

void BoundMembers::Move(
    const Followers& followers, Observable* from,
    const ErasedReference& names) {
  // A wait reached through one of the members follows it. One reached at the
  // same storage by any other way stays, which is what keeps a force on a sink
  // from being an occurrence for whoever watches its source.
  if (from != nullptr) {
    std::vector<WaitMembership*> through;
    from->Members().ForEach([&](WaitMembership& membership) {
      if (std::ranges::find(followers.naming, membership.through) !=
          followers.naming.end()) {
        through.push_back(&membership);
      }
    });
    Observable* to = names.ReportsTo();
    for (WaitMembership* membership : through) {
      membership->Unlink();
      if (to != nullptr) {
        to->Members().PushBack(*membership);
      }
    }
  }
  const auto name = [&](ErasedReference& reference) {
    reference.holder = names.holder;
    reference.storage = names.storage;
    reference.whole = names.whole;
  };
  for (ErasedReference* each : followers.naming) {
    name(*each);
  }
  for (ErasedReference* driver : followers.forced_below) {
    name(*driver);
  }
}

void BoundMembers::Force(
    ErasedReference& member, const ErasedReference& forced,
    std::shared_ptr<void> storage) {
  Member& state = members_[&member];
  state.driver = std::make_unique<ErasedReference>(member);
  state.forced = std::move(storage);
  Move(Following(member), member.ReportsTo(), forced);
}

auto BoundMembers::StillForcing(
    const ErasedReference& member, std::int64_t generation) const -> bool {
  const auto at = members_.find(&member);
  return at != members_.end() && at->second.driver != nullptr &&
         at->second.generation == generation;
}

auto BoundMembers::DriverOf(const ErasedReference& member) const
    -> ErasedReference {
  const auto at = members_.find(&member);
  if (at == members_.end() || at->second.driver == nullptr) {
    return member;
  }
  return *at->second.driver;
}

void BoundMembers::Release(ErasedReference& member) {
  const auto at = members_.find(&member);
  // Releasing what nothing ever forced has no effect (LRM 10.6.2).
  if (at == members_.end() || at->second.driver == nullptr) {
    return;
  }
  Member& state = at->second;
  ++state.generation;
  Move(Following(member), member.ReportsTo(), *state.driver);
  state.driver.reset();
  state.forced.reset();
}

auto ForcedMember(const ErasedReference& reference, std::int64_t level)
    -> ErasedReference& {
  if (TakeoverLevelOf(level) != support::TakeoverLevel::kForce) {
    throw InternalError(
        "ForcedMember: an `assign` names a member that owns no storage, which "
        "the front end refuses");
  }
  if (reference.member == nullptr) {
    throw InternalError(
        "ForcedMember: a force names a reference that was bound into no "
        "member");
  }
  return *reference.member;
}

}  // namespace lyra::runtime
