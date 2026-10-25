#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/scope_class.hpp"

namespace lyra::hir {

// A member a scope class published, whose type makes it a set of objects of
// another unit (LRM 23.3.2, 25.3): the same scope class and the same position
// counted out of the same signature as the leaf ending on a published member,
// reaching pointers rather than a cell. Which kind of object a select reaches
// is what the member's type states at that position, so the step does not
// restate it. The position is what crosses, never the name, because the name
// was resolved where the referrer compiles.
struct PublishedMemberRef {
  ScopeClassId scope_class;
  PublishedMemberId member;

  auto operator==(const PublishedMemberRef&) const -> bool = default;
};

// The block of a loop generate of a published scope whose index stood at
// `index` (LRM 27.4): the loop by its position among the scope's loops, and
// `result_class`, the class that block was published as.
// Where among the loop's blocks it stands is the holding object's to answer,
// since it is what ran the loop.
struct PublishedGenerateLoopRef {
  ScopeClassId scope_class;
  PublishedLoopId loop;
  std::int64_t index = 0;
  ScopeClassId result_class;

  auto operator==(const PublishedGenerateLoopRef&) const -> bool = default;
};

// The one block a generate construct of a published scope holds when it builds
// at most one (LRM 27.5): the construct by its position among the scope's
// constructs of that kind, and `result_class`, the class that block was
// published as.
struct PublishedGenerateBlockRef {
  ScopeClassId scope_class;
  PublishedChoiceId choice;
  ScopeClassId result_class;

  auto operator==(const PublishedGenerateBlockRef&) const -> bool = default;
};

// What a path element names on a published scope class, whichever unit
// published it -- this one's included, where a hierarchical name reaches
// another instance of it (LRM 23.6). What it lands on is a published scope
// class as well, so the path goes on against what that class published.
using PublishedScopeRef = std::variant<
    PublishedMemberRef, PublishedGenerateLoopRef, PublishedGenerateBlockRef>;

// An element of a path through published scope classes. A descent from the
// instance a virtual interface holds is made of these alone (LRM 25.9), since
// everything below that instance is what its interface published.
using PublishedStep = PathElement<PublishedScopeRef>;

}  // namespace lyra::hir
