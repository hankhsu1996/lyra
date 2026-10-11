#include "lyra/runtime/observable.hpp"

#include <cstdint>
#include <map>
#include <memory>
#include <vector>

#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/runtime_effects.hpp"
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

void Observable::WakeAffectedBy(
    RuntimeEffects& effects, const PartPath& written, const Change& change) {
  IntrusiveList<WaitMembership>* members = &members_;
  Children* children = children_.get();
  for (const std::uint32_t step : written.Steps()) {
    effects.WakeParkedOn(*members, Change::Elements(Only(step)));
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
  const Change only_reached = Change::Whole();
  const Change& told = written.IsInexact() ? only_reached : change;
  effects.WakeParkedOn(*members, told);
  if (children == nullptr) {
    return;
  }
  // Everything under a node, told that it was reached.
  const auto wake_under = [&](const auto& self, Node& node) -> void {
    effects.WakeParkedOn(node.members, only_reached);
    if (node.children == nullptr) {
      return;
    }
    for (auto& [step, inside] : node.children->by_step) {
      self(self, inside);
    }
  };
  for (auto& [step, node] : children->by_step) {
    if (told.KnownUnchanged(Only(step))) {
      continue;
    }
    wake_under(wake_under, node);
  }
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
