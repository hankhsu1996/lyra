#pragma once

#include <vector>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/signature_member_step.hpp"

namespace lyra::hir {

// The instance a virtual interface holds, or an interface that one
// instantiates, reached as a whole (LRM 25.9): the value another virtual
// interface is assigned, or the object one of its subroutines is called on.
// `handle` is the virtual interface, and `object` this unit's record of what
// the interface its type names published. `steps` descend through the
// interfaces that one instantiates, each counted out of what the unit before it
// published. The type of the virtual interface names that unit, so every
// position is settled where this unit compiles and only the instance is chosen
// when the access runs -- which is also when a handle holding null fails, as
// the standard requires.
struct InterfaceInstanceAccessExpr {
  ExprId handle;
  ExternalUnitObjectId object;
  std::vector<SignatureMemberStep> steps;

  auto operator<=>(const InterfaceInstanceAccessExpr&) const = default;
};

// A member of an instance reached through a virtual interface, at the position
// the name was counted out of in the unit the descent lands on.
//
// An expression reads and writes the member through it, and a wait watches the
// member through the same designation: which instance's variable that is, is
// known each time the wait evaluates the handle.
struct InterfaceMemberAccessExpr {
  InterfaceInstanceAccessExpr instance;
  PublishedMemberId member;

  auto operator<=>(const InterfaceMemberAccessExpr&) const = default;
};

}  // namespace lyra::hir
