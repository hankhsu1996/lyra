#include "lyra/runtime/bound_members.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

BoundMembers::BoundMembers() = default;
BoundMembers::~BoundMembers() = default;

void BoundMembers::Bind(ErasedReference& member, const ErasedReference& bound) {
  member = bound;
  member.member = &member;
  if (bound.member != nullptr) {
    members_[bound.member].bound_from_it.push_back(&member);
  }
}

auto BoundMembers::BeginForce(ErasedReference& member) -> std::int64_t {
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

void BoundMembers::Retarget(
    ErasedReference& member, const ErasedReference& forced,
    std::int64_t generation) {
  Member& state = members_[&member];
  if (state.generation != generation) {
    return;
  }
  if (state.driver == nullptr) {
    state.driver = std::make_unique<ErasedReference>(member);
  }
  Move(Following(member), member.ReportsTo(), forced);
  // An earlier force's evaluation learns here that it is superseded: nothing
  // names its storage any longer.
  if (state.ended != nullptr) {
    current_runtime().WakeParkedOn(state.ended->Members(), Change::Whole());
  }
}

auto BoundMembers::StillForcing(
    const ErasedReference& member, std::int64_t generation) const -> bool {
  const auto at = members_.find(&member);
  return at != members_.end() && at->second.driver != nullptr &&
         at->second.generation == generation;
}

auto BoundMembers::ForceEnded(ErasedReference& member) -> Observable& {
  Member& state = members_[&member];
  if (state.ended == nullptr) {
    state.ended = std::make_unique<Observable>();
  }
  return *state.ended;
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
  if (at == members_.end()) {
    return;
  }
  Member& state = at->second;
  // A force begun and not yet evaluated is ended here too.
  ++state.generation;
  if (state.driver == nullptr) {
    return;
  }
  Move(Following(member), member.ReportsTo(), *state.driver);
  state.driver.reset();
  if (state.ended != nullptr) {
    current_runtime().WakeParkedOn(state.ended->Members(), Change::Whole());
  }
}

auto BoundMember(const ErasedReference& reference) -> ErasedReference& {
  if (reference.member == nullptr) {
    throw InternalError(
        "BoundMember: a force names a reference that was bound into no member");
  }
  return *reference.member;
}

}  // namespace lyra::runtime
