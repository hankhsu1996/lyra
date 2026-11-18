#include "lyra/runtime/observable.hpp"

#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/kept_original.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope_exit.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

// One part some wait is enrolled at or under: the waits on it, and the nodes
// of the parts inside it.
struct Observable::Node {
  IntrusiveList<WaitMembership> members;
  std::unique_ptr<Children> children;
};

// The nodes one step below a part, by the step that reaches each. A node stays
// where it is made, since the waits enrolled at it point into it.
struct Observable::Children {
  std::map<std::uint32_t, Node> by_step;
};

namespace {

// The one element or member `step` reaches, as a change names positions.
auto Only(std::uint32_t step) -> value::BitPositions {
  return {.lsb = step, .width = 1};
}

}  // namespace

Observable::Observable() = default;
Observable::~Observable() = default;

auto Observable::MembersAt(const PartPath& path)
    -> IntrusiveList<WaitMembership>& {
  IntrusiveList<WaitMembership>* members = &members_;
  std::unique_ptr<Children>* children = &children_;
  for (const std::uint32_t step : path.Steps()) {
    if (*children == nullptr) {
      *children = std::make_unique<Children>();
    }
    Node& child = (*children)->by_step[step];
    members = &child.members;
    children = &child.children;
  }
  return *members;
}

// Tells the waits on the part at `at`, on each part holding it, and on each
// part inside it, `change` being what happened to the part. Each part holding
// it is told which of its elements or members that was, and each part inside
// it only that it was reached, those inside elements the change shows
// untouched excepted. `tell` tells the memberships of one part what can be
// said of what moved there.
template <class Tell>
void Observable::TellAround(
    const PartPath& at, const Change& change, Tell tell) {
  IntrusiveList<WaitMembership>* members = &members_;
  Children* children = children_.get();
  for (const std::uint32_t step : at.Steps()) {
    tell(*members, Change::Elements(Only(step)));
    if (children == nullptr) {
      return;
    }
    const auto child = children->by_step.find(step);
    if (child == children->by_step.end()) {
      return;
    }
    members = &child->second.members;
    children = child->second.children.get();
  }
  tell(*members, change);
  if (children == nullptr) {
    return;
  }
  const Change only_reached = Change::Whole();
  const auto tell_under = [&](const auto& self, Node& node) -> void {
    tell(node.members, only_reached);
    if (node.children == nullptr) {
      return;
    }
    for (auto& [step, inside] : node.children->by_step) {
      self(self, inside);
    }
  };
  for (auto& [step, node] : children->by_step) {
    if (!change.KnownUnchanged(Only(step))) {
      tell_under(tell_under, node);
    }
  }
}

void Observable::WakeAffectedBy(
    RuntimeEffects& effects, const PartPath& written, const Change& change,
    bool kept) {
  const Change only_reached = Change::Whole();
  const Change& told = written.IsInexact() ? only_reached : change;
  TellAround(
      written, told,
      [&](IntrusiveList<WaitMembership>& members, const Change& there) {
        effects.TellWrite(members, there, kept);
      });
}

void Observable::TellWhatWasLeft(
    RuntimeEffects& effects, const PartPath& at, const Change& change) {
  TellAround(
      at, change,
      [&](IntrusiveList<WaitMembership>& members, const Change& there) {
        effects.TellWhatTheEventLeft(members, there);
      });
}

void Observable::Enrol(const PartPath& path, WaitMembership& membership) {
  MembersAt(path).PushBack(membership);
  if (std::holds_alternative<CombinationalAwaiter*>(membership.observer) &&
      combinational_ == nullptr) {
    combinational_ = std::make_unique<CombinationalWatch>();
    combinational_->place = this;
  }
}

namespace {

// No part kept lies in another, so a kept part holding `written` is the last
// one ordered no later than it: any between the two would lie in the first.
auto Holds(const KeptParts& kept, const PartPath& written) -> bool {
  auto at_or_before = kept.upper_bound(written);
  if (at_or_before == kept.begin()) {
    return false;
  }
  --at_or_before;
  return at_or_before->first.IsPrefixOf(written);
}

// The entries of `kept` at the part `at` and inside it, which come right after
// it.
template <class Kept>
auto Inside(Kept& kept, const PartPath& at) {
  const auto inside = kept.lower_bound(at);
  auto past_inside = inside;
  while (past_inside != kept.end() && at.IsPrefixOf(past_inside->first)) {
    ++past_inside;
  }
  return std::ranges::subrange(inside, past_inside);
}

// Keeps the part at `at` in `watch`, which holds nothing holding it. What was
// kept of the parts and bits inside it is what they held before the event's
// execution, so they are put back while this part is copied and taken out
// again after.
void KeepIn(CombinationalWatch& watch, const PartPath& at, MakeOriginal make) {
  const auto parts = Inside(watch.kept, at);
  const auto bits = Inside(watch.bits, at);
  const auto exchange = [&] {
    for (const auto& entry : parts) {
      entry.second->Exchange();
    }
    for (auto& entry : bits) {
      entry.second.Exchange();
    }
  };
  exchange();
  std::unique_ptr<KeptOriginal> original;
  {
    const ScopeExit put_back(exchange);
    original = make();
  }
  watch.bits.erase(bits.begin(), bits.end());
  watch.kept.emplace_hint(
      watch.kept.erase(parts.begin(), parts.end()), at, std::move(original));
}

}  // namespace

// The place is on the run's list exactly while it keeps something, so it is
// put there by the first thing it keeps.
void Observable::KeepOriginalBy(const PartPath& at, MakeOriginal make) {
  CombinationalWatch& watch = *combinational_;
  if (at.IsInexact() || Holds(watch.kept, at)) {
    return;
  }
  const bool first = watch.kept.empty() && watch.bits.empty();
  KeepIn(watch, at, make);
  if (first) {
    current_runtime().JudgeOnceTheEventIsExecuted(watch);
  }
}

void Observable::KeepBitsOriginal(
    const PartPath& at, value::IntegralView live, value::BitPositions bits) {
  if (!WatchedCombinationally()) {
    return;
  }
  CombinationalWatch& watch = *combinational_;
  if (at.IsInexact() || Holds(watch.kept, at)) {
    return;
  }
  const bool first = watch.kept.empty() && watch.bits.empty();
  watch.bits.try_emplace(at, live).first->second.Keep(bits);
  if (first) {
    current_runtime().JudgeOnceTheEventIsExecuted(watch);
  }
}

auto Observable::KeepsOriginalOf(const PartPath& written) const -> bool {
  return Holds(combinational_->kept, written) ||
         combinational_->bits.contains(written);
}

auto Observable::KeepsWhatWasWritten(
    const PartPath& written, const Change& change) const -> bool {
  if (!WatchedCombinationally()) {
    return false;
  }
  if (KeepsOriginalOf(written)) {
    return true;
  }
  const std::optional<value::BitPositions> elements = change.ElementsReached();
  if (!elements.has_value()) {
    return false;
  }
  for (std::uint64_t ordinal = elements->lsb;
       ordinal < elements->lsb + elements->width; ++ordinal) {
    if (!KeepsOriginalOf(written.Join(
            PartPath::Element(static_cast<std::int64_t>(ordinal))))) {
      return false;
    }
  }
  return true;
}

auto Observable::EveryEnrolled() -> std::vector<Enrolled> {
  std::vector<Enrolled> enrolled;
  const auto collect = [&](const PartPath& path,
                           IntrusiveList<WaitMembership>& members) {
    members.ForEach([&](WaitMembership& membership) {
      enrolled.push_back({.path = path, .membership = &membership});
    });
  };
  const auto collect_under = [&](const auto& self, const PartPath& parent,
                                 Children* children) -> void {
    if (children == nullptr) {
      return;
    }
    for (auto& [step, node] : children->by_step) {
      const PartPath path = parent.Join(PartPath::Child(step));
      collect(path, node.members);
      self(self, path, node.children.get());
    }
  };
  const PartPath whole;
  collect(whole, members_);
  collect_under(collect_under, whole, children_.get());
  return enrolled;
}

}  // namespace lyra::runtime
