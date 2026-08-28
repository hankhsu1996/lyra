#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/base/arena.hpp"
#include "lyra/hir/published_member.hpp"

namespace lyra::hir {

struct ExternalUnitObjectId {
  std::uint32_t value;

  auto operator<=>(const ExternalUnitObjectId&) const
      -> std::strong_ordering = default;
};

// The object of a unit this one references, as that unit's signature promised
// it: which unit defines it, the class an instance of it is, and the members it
// published, in the order that fixes where their storage sits. The member types
// are this unit's own -- taken into its pool where the signature was consumed
// -- so nothing below this record reads a signature or a type it does not own.
//
// This unit compiles none of it; it holds what it compiled against.
struct ExternalUnitObject {
  std::string unit_name;
  std::string class_name;
  base::Arena<PublishedMember, PublishedMemberId> members;
};

}  // namespace lyra::hir
