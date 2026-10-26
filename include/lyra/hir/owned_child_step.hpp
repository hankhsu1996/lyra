#pragma once

#include <compare>
#include <cstdint>
#include <vector>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

struct GenerateId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const GenerateId&) const -> std::strong_ordering = default;
};

// A body a generate block of this unit compiles to, by its place among the
// unit's block bodies. Blocks stating the same thing are one body, whichever
// object builds them.
struct BlockBodyId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const BlockBodyId&) const -> std::strong_ordering = default;
};

struct InstanceMemberId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InstanceMemberId&) const
      -> std::strong_ordering = default;
};

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

// An element naming an instance a scope of this unit declares, by the declaring
// scope's own identity for it.
using OwnedChildStep = PathElement<InstanceMemberId>;

}  // namespace lyra::hir
