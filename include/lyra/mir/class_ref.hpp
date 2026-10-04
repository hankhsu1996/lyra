#pragma once

#include <optional>
#include <string>
#include <variant>

#include "lyra/mir/behavior_ordinal.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/runtime_class.hpp"

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

// The root every object of the design hierarchy extends: extending it is what
// roots an object in the runtime's tree (LRM 23.3). No compilation unit
// declares it, and each target realizes it in its own terms.
//
// A reference names a class and nothing else. How the runtime drives an object
// is a property of whichever class supplies the bodies it drives one through,
// so it is stated by that class rather than by any reference to a base.
struct ObjectTreeRootRef {
  auto operator==(const ObjectTreeRootRef&) const -> bool = default;
};

// The root every object the program builds extends where its class extends
// nothing the source wrote (LRM 8.13): what makes it an object the simulator
// owns and a handle can name. It is not a class of the design hierarchy, so
// extending it roots nothing in the runtime's tree, and each target realizes it
// in its own terms.
struct ManagedObjectRootRef {
  auto operator==(const ManagedObjectRootRef&) const -> bool = default;
};

// A reference to the class an object extends: one this unit declares, one
// another unit declares, the root of the design hierarchy, or the root of every
// object the program builds. They are reached differently -- a registry lookup,
// a name resolved against a consumed signature, a target's own realization of
// either root -- so each is its own arm.
using ClassRef = std::variant<
    IntraUnitClassRef, CrossUnitClassRef, ObjectTreeRootRef,
    ManagedObjectRootRef>;

// A reference to a class some compilation unit declares: this one or another.
// Only such a class has a declaration a unit states -- a definition the
// declaring unit emits, a constructor an object is built through -- because the
// two roots are the library's and there before any unit is. So whatever needs
// one of those names a class this way.
using DeclaredClassRef = std::variant<IntraUnitClassRef, CrossUnitClassRef>;

[[nodiscard]] inline auto AsClassRef(const DeclaredClassRef& declared)
    -> ClassRef {
  return std::visit([](const auto& ref) -> ClassRef { return ref; }, declared);
}

// The class a value of type `object` is an object of -- which is how a
// construction of that value names what it builds. Only a class some unit
// declares is ever built, so a type naming any other object, or no object, is a
// producer's defect.
[[nodiscard]] auto ClassOfObject(const TypePool& types, TypeId object)
    -> DeclaredClassRef;

// The object a value of type `reaches` reaches, as the class it is reached as:
// the class a handle is of, what a pointer points at, or the class a write in
// progress is open on. A value of any other type reaches no object, so
// asking is a producer's defect.
[[nodiscard]] auto ObjectReachedThrough(const TypePool& types, TypeId reaches)
    -> TypeId;

// A method that introduces a new virtual dispatch slot on the class it
// declares -- LRM 8.20 `virtual function` first appearance in an inheritance
// chain. The slot's canonical identity is this method's own declaration
// identity, because a slot carries nothing beyond what the introducer's
// declaration holds -- a name, a signature, participation in dispatch. Which
// behavior answers an interface class's slot is stated by the class answering
// it, not by the slot.
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
// another compilation unit -- LRM 8.20 across the unit boundary. The behavior's
// canonical identity carries no unit-local ids: it names the declaring unit and
// the introducing class's canonical name, together with which of that class's
// introductions it is, counted out of what that class published. That is the
// same coordinate an intra-unit override carries, with the class named by its
// parts rather than by an id.
struct OverridesExternalSlot {
  std::string unit_name;
  std::string class_name;
  BehaviorOrdinal ordinal;

  auto operator==(const OverridesExternalSlot&) const -> bool = default;
};

// A method that overrides a virtual function a class of the runtime library
// declares for the classes extending it: what a scope does in one of the
// phases the library drives it through.
struct OverridesLibraryVirtual {
  support::LibraryVirtual function;

  auto operator==(const OverridesLibraryVirtual&) const -> bool = default;
};

// A method's participation in the class-object dispatch table (LRM 8.20). A
// non-participating method (a regular direct-only callable) carries no value
// of this optional; a participating method carries the arm whose payload
// names the slot's canonical identity: an introducer names itself, an
// intra-unit override names the introducing (class, method) pair, a
// cross-unit override names the introducing (unit, class, method) name
// triple, and an override of the library's names the library's function. A
// consumer reads the slot's identity in one step.
using VirtualDispatchRole = std::variant<
    IntroducesVirtualSlot, OverridesIntraUnitSlot, OverridesExternalSlot,
    OverridesLibraryVirtual>;

// Whether this participation is the appearance that introduces the slot (LRM
// 8.20), as against overriding one an ancestor already declared. A callable in
// no dispatch introduces nothing, so a caller needing to tell overriding one
// from joining no dispatch at all asks whether the role is there before asking
// this.
[[nodiscard]] auto IntroducesSlot(
    const std::optional<VirtualDispatchRole>& role) -> bool;

}  // namespace lyra::mir
