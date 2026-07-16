#pragma once

#include <string>
#include <variant>

#include "lyra/mir/class_id.hpp"
#include "lyra/mir/method_id.hpp"

namespace lyra::mir {

// A reference to a class in this compilation unit's own class registry, named
// by its canonical local identity. The referred class is defined in this unit;
// a consumer reads its declaration through the registry. Used when an SV
// class extends another class of the same unit (LRM 8.13).
struct IntraUnitClassRef {
  ClassId class_id;

  auto operator==(const IntraUnitClassRef&) const -> bool = default;
};

// A reference to a class declared outside this compilation unit, named by its
// final target-language qualified name (e.g. `lyra::runtime::Scope`). One arm
// covers every non-intra-unit class -- a runtime library base and a
// cross-unit SV class both flow through it, distinguished only by which
// qualified name the producer supplied. The backend renders it through the
// same type-mapping dispatch that renders `ExternalClassType`.
struct ExternalClassRef {
  std::string qualified_name;

  auto operator==(const ExternalClassRef&) const -> bool = default;
};

// A reference to the class an object extends. Either intra-unit (a class of
// this unit's registry) or external (any other class, named by its qualified
// target-language name). A consumer reads each arm directly.
using ClassRef = std::variant<IntraUnitClassRef, ExternalClassRef>;

// A method that introduces a new virtual dispatch slot on the class it
// declares -- LRM 8.20 `virtual function` first appearance in an inheritance
// chain. The slot's canonical identity is this method's own declaration
// identity: as long as a dispatch slot carries no state beyond what the
// introducer's declaration already holds (a name, a signature, participation
// in dispatch), aliasing "slot identity" to "introducer's (class, method)"
// is a chosen simplification, not a natural fact. When a slot gains
// independent metadata -- a pure/abstract requirement, a final marker,
// interface conformance -- a distinct `SlotId` becomes the right shape.
struct IntroducesVirtualSlot {
  auto operator==(const IntroducesVirtualSlot&) const -> bool = default;
};

// A method that overrides a virtual dispatch slot declared by a class of this
// same compilation unit -- LRM 8.20. The stored (`slot_owner`, `slot_id`) is
// the slot's canonical identity: the class and method-arena position where
// the slot was originally introduced, invariant across every override in the
// chain. A call site names the slot in one read; no consumer walks an
// override chain to derive it.
struct OverridesIntraUnitSlot {
  ClassId slot_owner;
  MethodId slot_id;

  auto operator==(const OverridesIntraUnitSlot&) const -> bool = default;
};

// A method's participation in the class-object dispatch table (LRM 8.20). A
// non-participating method (a regular direct-only callable) carries no value
// of this optional; a participating method carries the arm whose payload
// names the slot's canonical identity: an introducer names itself, an
// intra-unit override names the introducing (class, method) pair. A
// consumer reads the slot's identity in one step.
using VirtualDispatchRole =
    std::variant<IntroducesVirtualSlot, OverridesIntraUnitSlot>;

}  // namespace lyra::mir
