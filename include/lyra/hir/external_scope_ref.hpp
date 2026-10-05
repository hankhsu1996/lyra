#pragma once

#include <variant>

#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"

namespace lyra::hir {

// A member another unit published, whose type makes it an object of a third
// unit (LRM 25.3): the same record and the same position counted out of the
// same signature as the leaf ending on a published member, reaching a pointer
// rather than a cell, and `result_class`, this unit's record of the class that
// object is. The position is what crosses, never the name, because the name
// was resolved where the referrer compiles.
struct ExternalMemberRef {
  ExternalScopeClassId scope_class;
  PublishedMemberId member;
  ExternalScopeClassId result_class;

  auto operator==(const ExternalMemberRef&) const -> bool = default;
};

// A generate construct of a scope another unit published (LRM 27), by its
// position among the scope's constructs, and `result_class`, this unit's record
// of the class the block the path element reaches was published as. A loop
// holds the blocks it counted out and the element's select picks one (LRM
// 27.4); a construct that builds at most one holds that one, and the element
// names it by its label with no select (LRM 27.5).
struct ExternalGenerateRef {
  ExternalScopeClassId scope_class;
  PublishedGenerateId generate;
  ExternalScopeClassId result_class;

  auto operator==(const ExternalGenerateRef&) const -> bool = default;
};

// What a path element names on a scope class another unit published. What it
// lands on is a published scope class as well, so the path goes on against
// what that class published.
using ExternalScopeRef = std::variant<ExternalMemberRef, ExternalGenerateRef>;

// An element of a path through scope classes other units published. A descent
// from the instance a virtual interface holds is made of these alone (LRM
// 25.9), since everything below that instance is what its interface published.
using ExternalStep = PathElement<ExternalScopeRef>;

}  // namespace lyra::hir
