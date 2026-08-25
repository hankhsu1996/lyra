#pragma once

#include <compare>
#include <cstdint>

namespace lyra::base {

// Which member of a structural aggregate: the declaration-order position of a
// tuple element, a union member, a tagged union's tag, or one step of a
// designator path.
//
// This is not an identity, and the difference is the reason it exists. An
// identity is conferred: two separately declared things are distinct however
// alike they look, so something has to mint the distinction and remember it. A
// structural type is the opposite -- two aggregates with the same members are
// the same type -- which is what lets such a type be interned, and what forbids
// its members from carrying minted identities, since a mint would make two
// identical types differ. All that is left to name a member by is where it
// sits, and where it sits follows from the type's own content rather than from
// anyone's grant.
//
// Two things follow, and together they are why this is a type of its own rather
// than a bare integer. A position needs no translation across a lowering
// boundary: the layers agree about it because they describe the same structure,
// not because a table was kept between them. And a position is arithmetic -- it
// compares, it orders, it indexes -- where an identity is none of those, so the
// two must not share a spelling that lets either be passed where the other is
// meant.
struct ComponentIndex {
  std::uint32_t value;

  auto operator<=>(const ComponentIndex&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::base
