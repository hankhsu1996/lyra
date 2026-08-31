#pragma once

#include <string>
#include <variant>

#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"

namespace lyra::mir {

// A reference to a class in this compilation unit's own class registry, named
// by its canonical local identity. The referred class is defined in this unit;
// a consumer reads its declaration through the registry. Used when an SV
// class extends another class of the same unit (LRM 8.13).
struct IntraUnitClassRef {
  ClassId class_id;

  auto operator==(const IntraUnitClassRef&) const -> bool = default;
};

// A reference to a class another compilation unit declares, named the way every
// cross-unit name is: by the declaring unit and the class's canonical name. The
// pair is the identity; how it is spelled belongs to whichever target a backend
// emits, so nothing here composes one.
struct CrossUnitClassRef {
  std::string unit_name;
  std::string class_name;

  auto operator==(const CrossUnitClassRef&) const -> bool = default;
};

// A reference to a class the runtime library defines, named by the library
// symbol itself. No compilation unit declares it, so there is no unit to name
// it through and nothing to resolve: the symbol is the identity.
struct RuntimeClassRef {
  std::string symbol;

  auto operator==(const RuntimeClassRef&) const -> bool = default;
};

// A reference to the class an object extends: one this unit declares, one
// another unit declares, or one the runtime library provides. The three are
// reached differently -- a registry lookup, a name resolved against a consumed
// signature, a library symbol -- so each is its own arm.
using ClassRef =
    std::variant<IntraUnitClassRef, CrossUnitClassRef, RuntimeClassRef>;

// A method that introduces a new virtual dispatch slot on the class it
// declares -- LRM 8.20 `virtual function` first appearance in an inheritance
// chain. The slot's canonical identity is this method's own declaration
// identity: as long as a dispatch slot carries no state beyond what the
// introducer's declaration already holds (a name, a signature, participation
// in dispatch), aliasing "slot identity" to "introducer's (class, method)"
// is a chosen simplification, not a natural fact. When a slot gains
// independent metadata -- a pure/abstract requirement, a final marker,
// interface conformance -- it needs an identity of its own.
struct IntroducesVirtualSlot {
  auto operator==(const IntroducesVirtualSlot&) const -> bool = default;
};

// A method that overrides a virtual dispatch slot declared by a class of this
// same compilation unit -- LRM 8.20. The stored (`slot_owner`, `slot_id`) is
// the slot's canonical identity: the class and callable-arena position where
// the slot was originally introduced, invariant across every override in the
// chain. A call site names the slot in one read; no consumer walks an
// override chain to derive it.
struct OverridesIntraUnitSlot {
  ClassId slot_owner;
  CallableId slot_id;

  auto operator==(const OverridesIntraUnitSlot&) const -> bool = default;
};

// A method that overrides a virtual dispatch slot introduced by a class in
// another compilation unit -- LRM 8.20 across the unit boundary. The slot's
// canonical identity carries no unit-local ids: it names the declaring unit,
// the introducing class's canonical name, and the introducing method's source
// name. A virtual slot has no independent name of its own; its canonical
// identity in a by-name world is the introducing method's source name.
// A backend renders the override through the target language's own virtual-
// override machinery -- the same shape as an intra-unit override -- reached
// by including the declaring unit's header.
struct OverridesExternalSlot {
  std::string unit_name;
  std::string class_name;
  std::string method_name;

  auto operator==(const OverridesExternalSlot&) const -> bool = default;
};

// A method's participation in the class-object dispatch table (LRM 8.20). A
// non-participating method (a regular direct-only callable) carries no value
// of this optional; a participating method carries the arm whose payload
// names the slot's canonical identity: an introducer names itself, an
// intra-unit override names the introducing (class, method) pair, a
// cross-unit override names the introducing (unit, class, method) name
// triple. A consumer reads the slot's identity in one step.
using VirtualDispatchRole = std::variant<
    IntroducesVirtualSlot, OverridesIntraUnitSlot, OverridesExternalSlot>;

}  // namespace lyra::mir
