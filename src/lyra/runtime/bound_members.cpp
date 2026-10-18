#include "lyra/runtime/bound_members.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/source_follower.hpp"
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
  const Followers followers = Following(member);
  NameAliases(followers, member);
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
  // A member that already names storage -- a net's, which names its own value
  // until its driver's turn -- has waits through it, which follow.
  Move(followers, member.ReportsTo(), bound);
}

void BoundMembers::Alias(ErasedReference& alias, const ErasedReference& bound) {
  // Where `bound` names a member, the alias is a name of that member: it is
  // kept naming what the member names, and a takeover through it is the
  // member's. Where `bound` is not bound yet, the alias waits on it and learns
  // what it is a name of once `bound` is bound.
  if (bound.storage == nullptr) {
    members_[&bound].bound_from_it.push_back(&alias);
    return;
  }
  alias.member = bound.member;
  if (bound.member != nullptr) {
    members_[bound.member].bound_from_it.push_back(&alias);
  }
  const Followers followers = Following(alias);
  if (alias.member != nullptr) {
    NameAliases(followers, *alias.member);
  }
  Move(followers, alias.ReportsTo(), bound);
}

void BoundMembers::NameAliases(
    const Followers& followers, ErasedReference& member) {
  // A member names itself from its own binding, so a follower not yet named is
  // an alias made before what it follows was bound.
  for (ErasedReference* follower : followers.naming) {
    if (follower->member == nullptr) {
      follower->member = &member;
    }
  }
}

auto BoundMembers::Forced(const ErasedReference& member) const -> bool {
  const auto at = members_.find(&member);
  return at != members_.end() && at->second.driver != nullptr;
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
  // A wait reached through one of the members follows it, and so does what
  // follows the driver of a member below that a force of its own covers. One
  // reached at the same storage by any other way stays, which is what keeps a
  // force on a sink from being an occurrence for whoever watches its source.
  if (from != nullptr) {
    // Each goes on watching, of what the member names now, the part it
    // watched of what the member named.
    Observable* to = names.ReportsTo();
    const auto follows = [&](const ErasedReference* through) {
      return std::ranges::find(followers.naming, through) !=
                 followers.naming.end() ||
             std::ranges::find(followers.forced_below, through) !=
                 followers.forced_below.end();
    };
    for (const Observable::Enrolled& enrolled : from->EveryEnrolled()) {
      WaitMembership& membership = *enrolled.membership;
      if (!follows(membership.through)) {
        continue;
      }
      const PartPath watched =
          enrolled.path.StripPrefix(membership.through->part);
      membership.Unlink();
      if (to != nullptr) {
        to->MembersAt(names.part.Join(watched)).PushBack(membership);
      }
    }
  }
  const auto name = [&](ErasedReference& reference) {
    reference.holder = names.holder;
    reference.storage = names.storage;
    reference.whole = names.whole;
    reference.part = names.part;
  };
  for (ErasedReference* each : followers.naming) {
    name(*each);
  }
  for (ErasedReference* driver : followers.forced_below) {
    name(*driver);
  }
}

auto BoundMembers::ForcedStorage(const ErasedReference& member)
    -> std::shared_ptr<void>& {
  return members_[&member].forced;
}

void BoundMembers::Force(
    ErasedReference& member, const ErasedReference& forced,
    const std::function<OwnedCall(const ErasedReference& driver)>& follow) {
  Member& state = members_[&member];
  state.driver = std::make_unique<ErasedReference>(member);
  Move(Following(member), member.ReportsTo(), forced);
  // What drives the member is watched through its driver, so that a force on
  // what is bound above it, which renames that driver, moves the watch too.
  const ErasedReference& driver = *state.driver;
  Trigger source(driver.ReportsTo(), Observation{}, {}, 0, 0);
  source.through = &driver;
  source.part = driver.part;
  state.follower = std::make_unique<SourceFollower>(source, follow(driver));
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
  state.follower.reset();
  Move(Following(member), member.ReportsTo(), *state.driver);
  state.driver.reset();
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
