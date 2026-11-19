#include "lyra/runtime/observable.hpp"

#include <algorithm>
#include <cstdint>
#include <iterator>
#include <map>
#include <memory>
#include <ranges>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/runtime/interval_accumulator.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope_exit.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_change_wait.hpp"
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

void Observable::TellChange(
    RuntimeEffects& effects, const PartPath& written, const Change& change,
    bool accumulated) {
  const Change only_reached = Change::Whole();
  const Change& told = written.IsInexact() ? only_reached : change;
  TellAround(
      written, told,
      [&](IntrusiveList<WaitMembership>& members, const Change& there) {
        effects.TellChange(members, there, accumulated);
      });
}

void Observable::TellIntervalChange(
    RuntimeEffects& effects, const PartPath& at, const Change& change) {
  TellAround(
      at, change,
      [&](IntrusiveList<WaitMembership>& members, const Change& there) {
        effects.TellIntervalChange(members, there);
      });
}

void Observable::Enrol(const PartPath& path, WaitMembership& membership) {
  MembersAt(path).PushBack(membership);
  if (std::holds_alternative<PerIntervalAwaiter*>(membership.observer) &&
      accumulator_ == nullptr) {
    accumulator_ = std::make_unique<IntervalAccumulator>();
    accumulator_->place = this;
  }
}

namespace {

// No part kept lies in another, so a kept part holding `at` is the last one
// ordered no later than it: any between the two would lie in the first. An
// inexact path keeps the steps it has, so a part holding it is found the same
// way.
auto Holds(const std::vector<PartOriginalAt>& parts, const PartPath& at)
    -> bool {
  const auto at_or_before =
      std::ranges::upper_bound(parts, at, ByStep{}, &PartOriginalAt::at);
  return at_or_before != parts.begin() &&
         std::prev(at_or_before)->at.IsPrefixOf(at);
}

// The entries of `entries` at the part `at` and inside it, which come right
// after it.
template <class Entry>
auto Inside(std::vector<Entry>& entries, const PartPath& at) {
  const auto inside =
      std::ranges::lower_bound(entries, at, ByStep{}, &Entry::at);
  auto past_inside = inside;
  while (past_inside != entries.end() && at.IsPrefixOf(past_inside->at)) {
    ++past_inside;
  }
  return std::ranges::subrange(inside, past_inside);
}

// Keeps the original of the part at `at` in `accumulator`, which holds none of
// a part holding it, in bytes `storage` hands out. The originals of the parts
// and bits inside it are what they held before the interval first modified
// them, so they are put back while this part is copied and taken out again
// after.
void KeepIn(
    IntervalAccumulator& accumulator, const PartPath& at, CopyOriginal copy,
    EffectStorage& storage) {
  const auto parts = Inside(accumulator.parts, at);
  const auto bits = Inside(accumulator.bits, at);
  const auto exchange = [&] {
    for (const PartOriginalAt& entry : parts) {
      entry.original->Exchange();
    }
    for (BitsOriginalAt& entry : bits) {
      entry.bits.Exchange();
    }
  };
  exchange();
  OwnedOriginal original = [&] {
    const ScopeExit put_back(exchange);
    return copy(storage);
  }();
  accumulator.bits.erase(bits.begin(), bits.end());
  accumulator.parts.insert(
      accumulator.parts.erase(parts.begin(), parts.end()),
      PartOriginalAt{.at = at, .original = std::move(original)});
}

}  // namespace

namespace {

// Whether the open interval accumulated a modification of the part at
// `at` of the place `accumulator` keeps originals for, `keep` keeping the
// part's original where no part holding it is kept yet. The place is on the
// interval's list exactly while it keeps something, so it is put there by the
// first original it keeps. Only the execution of a process whose updates
// coalesce opens an interval; any other modification is told as it lands.
template <class Keep>
auto AccumulateAt(
    IntervalAccumulator& accumulator, RuntimeEffects& effects,
    const PartPath& at, Keep keep) -> bool {
  if (!effects.IntervalOpen()) {
    return false;
  }
  if (at.IsInexact()) {
    return Holds(accumulator.parts, at);
  }
  if (!Holds(accumulator.parts, at)) {
    const bool first = accumulator.Empty();
    keep();
    if (first) {
      effects.JoinInterval(accumulator);
    }
  }
  return true;
}

}  // namespace

auto Observable::Accumulate(const PartPath& at, CopyOriginal copy) -> bool {
  RuntimeEffects& effects = current_runtime();
  IntervalAccumulator& accumulator = *accumulator_;
  return AccumulateAt(accumulator, effects, at, [&] {
    KeepIn(accumulator, at, copy, effects.StorageForEffects());
  });
}

auto Observable::BeforeModifyBits(
    const PartPath& at, value::IntegralView live, value::BitPositions bits)
    -> bool {
  if (accumulator_ == nullptr) {
    return false;
  }
  IntervalAccumulator& accumulator = *accumulator_;
  return AccumulateAt(accumulator, current_runtime(), at, [&] {
    auto kept = std::ranges::lower_bound(
        accumulator.bits, at, ByStep{}, &BitsOriginalAt::at);
    if (kept == accumulator.bits.end() || kept->at != at) {
      kept = accumulator.bits.insert(
          kept, BitsOriginalAt{.at = at, .bits = BitsOriginal(live)});
    }
    kept->bits.Keep(bits);
  });
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
