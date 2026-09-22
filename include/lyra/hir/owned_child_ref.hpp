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

// A generate block (LRM 27) as a child of the scope that declares it: the
// generate construct it belongs to, plus which of that construct's elaborated
// blocks the name meant (LRM 27.4).
//
// The block is named and not the compiled scope, because how many scopes the
// construct compiled to is not something a name can be resolved against: a
// repeated structure whose blocks agree compiles to one, and the block a name
// meant becomes a coordinate on it, while blocks that disagree each compile to
// their own. Both answers reach the same elaborated block, so stating the
// block leaves the choice to whatever realizes the construct, and no name has
// to be spelled before that choice exists.
struct GenerateChildRef {
  GenerateId generate;
  std::uint32_t block;

  auto operator==(const GenerateChildRef&) const -> bool = default;
};

// A child object the referrer's compilation unit declares, named by the
// declaring scope's own identity for it.
using OwnedChildRef = std::variant<InstanceMemberId, GenerateChildRef>;

// One object reached through such a child. `indices` are the element
// coordinates the source named within it: an instance array is a single child
// spanning every element (LRM 23.3.2), so a coordinate picks the element out
// of it. A generate names its block on the child itself, so a generate step
// carries none.
//
// Every reach into a child of this unit names one of these, whether it is a
// navigation step of a reference or a hop of the descent a call takes to the
// scope declaring its callee: which object a name means is settled during
// elaboration either way, and a child that stands for several is the only
// thing that makes the coordinates load-bearing.
struct OwnedChildStep {
  OwnedChildRef child;
  std::vector<std::uint32_t> indices;

  auto operator==(const OwnedChildStep&) const -> bool = default;
};

}  // namespace lyra::hir
