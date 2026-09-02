#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/base/arena.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"

namespace lyra::hir {

struct ExternalUnitObjectId {
  std::uint32_t value;

  auto operator<=>(const ExternalUnitObjectId&) const
      -> std::strong_ordering = default;
};

// The object of a unit this one references, as that unit's signature promised
// it: which unit defines it, the class an instance of it is, the members it
// published in the order that fixes where their storage sits, and the
// subroutines it published. Every type here is this unit's own -- taken into
// its pool where the signature was consumed -- so nothing below this record
// reads a signature or a type it does not own.
//
// This unit compiles none of it; it holds what it compiled against.
struct ExternalUnitObject {
  std::string unit_name;
  std::string class_name;
  base::Arena<PublishedMember, PublishedMemberId> members;
  base::Arena<PublishedCallable, PublishedCallableId> callables;

  // The callable published under `name`, or nothing when the unit published no
  // such name. A name with no answer here is one the unit never promised, so a
  // call on it has nothing to compile against.
  [[nodiscard]] auto FindCallable(std::string_view name) const
      -> std::optional<PublishedCallableId> {
    for (const PublishedCallableId id : callables.Ids()) {
      if (callables.Get(id).name == name) return id;
    }
    return std::nullopt;
  }
};

}  // namespace lyra::hir
