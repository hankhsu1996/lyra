#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"

namespace lyra::hir {

// A member another unit published, whose type makes it a set of objects of a
// third unit (LRM 23.3.2, 25.3): the same record and the same position counted
// out of the same signature as the leaf ending on a published member, reaching
// pointers rather than a cell. Which kind of object a select reaches is what
// the member's type states at that position, so the step does not restate it.
// The position is what crosses, never the name, because the name was resolved
// where the referrer compiles.
struct ExternalMemberRef {
  ExternalScopeClassId scope_class;
  PublishedMemberId member;

  auto operator==(const ExternalMemberRef&) const -> bool = default;
};

// The block of a loop generate of a scope another unit published whose index
// stood at `index` (LRM 27.4): the loop by its position among the scope's
// loops, and `result_class`, this unit's record of the class that block was
// published as. Where among the loop's blocks it stands is the holding
// object's to answer, since it is what ran the loop.
struct ExternalGenerateLoopRef {
  ExternalScopeClassId scope_class;
  PublishedLoopId loop;
  std::int64_t index = 0;
  ExternalScopeClassId result_class;

  auto operator==(const ExternalGenerateLoopRef&) const -> bool = default;
};

// The one block a generate construct of a scope another unit published holds
// when it builds at most one (LRM 27.5): the construct by its position among
// the scope's constructs of that kind, and `result_class`, this unit's record
// of the class that block was published as.
struct ExternalGenerateBlockRef {
  ExternalScopeClassId scope_class;
  PublishedChoiceId choice;
  ExternalScopeClassId result_class;

  auto operator==(const ExternalGenerateBlockRef&) const -> bool = default;
};

// What a path element names on a scope class another unit published. What it
// lands on is a published scope class as well, so the path goes on against
// what that class published.
using ExternalScopeRef = std::variant<
    ExternalMemberRef, ExternalGenerateLoopRef, ExternalGenerateBlockRef>;

// An element of a path through scope classes other units published. A descent
// from the instance a virtual interface holds is made of these alone (LRM
// 25.9), since everything below that instance is what its interface published.
using ExternalStep = PathElement<ExternalScopeRef>;

}  // namespace lyra::hir
