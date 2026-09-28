#pragma once

#include <cstdint>
#include <vector>

#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/published_member.hpp"

namespace lyra::hir {

// One navigation step onto a member another unit published, whose type makes it
// an object of a third unit (LRM 25.3). It is the step form of the leaf that
// ends on a published member: the same record, the same position counted out of
// the same signature, reaching a pointer rather than a cell. `indices` pick one
// object out of a member standing for several. The position is what crosses,
// never the name -- a name identifies a step only where the route passes a
// signature, and this step lands on what one promised.
struct SignatureMemberStep {
  ExternalUnitObjectId object;
  PublishedMemberId member;
  std::vector<std::uint32_t> indices;

  auto operator<=>(const SignatureMemberStep&) const = default;
};

}  // namespace lyra::hir
