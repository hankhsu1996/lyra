#pragma once

#include <optional>
#include <string>
#include <variant>

#include "lyra/mir/behavior_ordinal.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::mir {

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

// Identity of a virtual dispatch slot introduced by a class in this
// compilation unit: the introducing class and the callable arena position
// where the slot was first declared. The receiver's dynamic type is what
// decides which implementation runs; the slot is the canonical logical
// identity a backend reads to reach the method's name and signature.
struct LocalVirtualSlot {
  ClassId owner_class;
  CallableId slot;

  auto operator==(const LocalVirtualSlot&) const -> bool = default;
};

// Identity of a behavior a class of another compilation unit introduced. The
// introducing class carries no unit-local id here, so it is named by (declaring
// unit, class path) together with which of that class's introductions
// this is, counted out of what it published -- the same coordinate an
// intra-unit slot carries, with the class named by its parts.
struct ExternalVirtualSlot {
  std::string unit_name;
  support::DefPath class_path;
  BehaviorOrdinal ordinal;

  auto operator==(const ExternalVirtualSlot&) const -> bool = default;
};

// The slot a virtual call and an override name: a class of this unit names it
// by position, and a class of another unit by the coordinate that unit
// published, since the identity follows the compilation-unit boundary.
using VirtualSlot = std::variant<LocalVirtualSlot, ExternalVirtualSlot>;

// A method that overrides a behavior an ancestor introduced (LRM 8.20), named
// by that behavior's slot: invariant across every override in the chain, so a
// call site names it in one read and no consumer walks an override chain.
struct OverridesVirtualSlot {
  VirtualSlot overridden;

  auto operator==(const OverridesVirtualSlot&) const -> bool = default;
};

// A method that overrides a virtual function a class of the runtime library
// declares for the classes extending it: what a scope does in one of the
// phases the library drives it through, or what an object does for its
// `randomize()` (LRM 18.6).
struct OverridesLibraryVirtual {
  support::LibraryVirtual function;

  auto operator==(const OverridesLibraryVirtual&) const -> bool = default;
};

// A method's participation in the class-object dispatch table (LRM 8.20). A
// non-participating method (a regular direct-only callable) carries no value
// of this optional; a participating method carries the arm whose payload
// names the slot's canonical identity: an introducer names itself, an override
// names the slot it overrides, and an override of the library's names the
// library's function. A consumer reads the slot's identity in one step.
using VirtualDispatchRole = std::variant<
    IntroducesVirtualSlot, OverridesVirtualSlot, OverridesLibraryVirtual>;

// Whether this participation is the appearance that introduces the slot (LRM
// 8.20), as against overriding one an ancestor already declared. A callable in
// no dispatch introduces nothing, so a caller needing to tell overriding one
// from joining no dispatch at all asks whether the role is there before asking
// this.
[[nodiscard]] auto IntroducesSlot(
    const std::optional<VirtualDispatchRole>& role) -> bool;

}  // namespace lyra::mir
