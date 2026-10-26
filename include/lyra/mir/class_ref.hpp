#pragma once

#include <optional>
#include <variant>

#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

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

// A reference to the class an object extends: a class some unit declares,
// which this unit names by the identity its registry gives it whichever unit
// defines it, the root of the design hierarchy, or the root of every object the
// program builds. A class is a registry entry and each root is a target's own
// realization, so each is its own arm.
using ClassRef = std::variant<ClassId, ObjectTreeRootRef, ManagedObjectRootRef>;

// The class a value of type `object` is an object of -- which is how a
// construction of that value names what it builds. Only a class some unit
// declares is ever built, so a type naming any other object, or no object, is a
// producer's defect.
[[nodiscard]] auto ClassOfObject(const TypePool& types, TypeId object)
    -> ClassId;

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
// it, not by the slot. Pure where the class declares it with no body (LRM
// 8.21), leaving it to what extends the class.
struct IntroducesVirtualSlot {
  bool is_pure = false;

  auto operator==(const IntroducesVirtualSlot&) const -> bool = default;
};

// Identity of a virtual dispatch slot: the class that introduced it and the
// callable arena position where it was first declared, whichever unit defines
// that class. The receiver's dynamic type is what decides which implementation
// runs; the slot is the canonical logical identity a backend reads to reach the
// method's name and signature.
struct VirtualSlot {
  ClassId owner_class;
  CallableId slot;

  auto operator==(const VirtualSlot&) const -> bool = default;
};

// A method that overrides a behavior an ancestor introduced (LRM 8.20), named
// by that behavior's slot: invariant across every override in the chain, so a
// call site names it in one read and no consumer walks an override chain.
// Pure where the class restates the behavior with no body (LRM 8.21).
struct OverridesVirtualSlot {
  VirtualSlot overridden;
  bool is_pure = false;

  auto operator==(const OverridesVirtualSlot&) const -> bool = default;
};

// A method's participation in the class-object dispatch table (LRM 8.20). A
// non-participating method (a regular direct-only callable) carries no value
// of this optional; a participating method carries the arm whose payload
// names the slot's canonical identity: an introducer names itself, and an
// override names the slot it overrides. A consumer reads the slot's identity in
// one step.
using VirtualDispatchRole =
    std::variant<IntroducesVirtualSlot, OverridesVirtualSlot>;

// Whether this participation is the appearance that introduces the slot (LRM
// 8.20), as against overriding one an ancestor already declared. A callable in
// no dispatch introduces nothing, so a caller needing to tell overriding one
// from joining no dispatch at all asks whether the role is there before asking
// this.
[[nodiscard]] auto IntroducesSlot(
    const std::optional<VirtualDispatchRole>& role) -> bool;

// Whether the role is a behavior the class leaves to what extends it (LRM
// 8.21).
[[nodiscard]] auto IsLeftAbstract(const VirtualDispatchRole& role) -> bool;

}  // namespace lyra::mir
