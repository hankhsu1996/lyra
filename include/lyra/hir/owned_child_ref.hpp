#pragma once

#include <compare>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

struct GenerateId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const GenerateId&) const -> std::strong_ordering = default;
};

struct StructuralScopeId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const StructuralScopeId&) const
      -> std::strong_ordering = default;
};

struct InstanceMemberId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InstanceMemberId&) const
      -> std::strong_ordering = default;
};

// The one block a generate construct holds when it builds at most one: a
// block standing on its own, or the one a conditional chose (LRM 27.5),
// identified by the position the source wrote it at among the construct's
// alternatives. That is the same number wherever the construct stands, which
// is what lets a name be resolved before anything knows which alternative a
// particular instance selected; a block standing on its own is the only
// alternative of its construct.
struct GenerateBlockRef {
  GenerateId generate;
  std::uint32_t alternative = 0;

  auto operator==(const GenerateBlockRef&) const -> bool = default;
};

// A child object the referrer's compilation unit declares, named by the
// declaring scope's own identity for it. A block of a loop is none: it is
// another body than whichever reaches it, so a path reaches it through what its
// loop's scope published.
using OwnedChildRef = std::variant<InstanceMemberId, GenerateBlockRef>;

// One element of a hierarchical path (LRM 23.6): what it names, and the
// instance selects written after it. Each select picks one instance out of
// what the name stands for -- an instance array (LRM 23.3.2) or an interface
// port carrying a range (LRM 25.3) -- and is the position of that instance,
// the value the source wrote being spent where the name resolves, as a
// declared range is. Something that stands for one object takes none, which is
// the empty case rather than a shape of its own.
template <typename Names>
struct PathElement {
  Names names;
  std::vector<std::uint32_t> selects;

  auto operator==(const PathElement&) const -> bool = default;
};

// An element naming a child of this unit. Every reach into one names it this
// way, whether it is a step of a reference or a hop of the descent a call
// takes to the scope declaring its callee.
using OwnedChildStep = PathElement<OwnedChildRef>;

}  // namespace lyra::hir
