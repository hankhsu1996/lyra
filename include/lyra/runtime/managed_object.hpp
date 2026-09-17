#pragma once

#include <cstdint>
#include <span>
#include <string_view>
#include <vector>

#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/storage_block.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

// One body of a class, as a code address with its prototype erased, so bodies
// of every signature share one table. What the call site restores it to is the
// signature the name it asked under carries -- one per dispatch position in
// every class filling it (LRM 8.20), one per declared name otherwise -- so the
// two sides cannot disagree about it. The same erasure, for the same reason,
// that a scope's exports use.
//
// Whatever answers with one of these hands back the address alone, so every
// body reached this way takes the object as its first parameter: nothing on the
// way converts it, and a body of a class extending another is entered with the
// same address a body of the base would be.
using ErasedMethodEntry = void (*)();

// The bodies a class fills its dispatch positions with (LRM 8.20), in position
// order and shared by every object of the class. It crosses as a pointer plus a
// length rather than a C++ container, like the storage schema beside it, so a
// definition can name a table that outlives whatever built it. An entry is null
// where nothing in the class's lineage supplied a body (LRM 8.21 pure virtual);
// no object carries one, because such a class is never constructed.
struct MethodDispatchTable {
  const ErasedMethodEntry* data = nullptr;
  std::uint32_t size = 0;

  constexpr MethodDispatchTable() = default;
  constexpr MethodDispatchTable(
      const ErasedMethodEntry* data, std::uint32_t size)
      : data(data), size(size) {
  }

  [[nodiscard]] constexpr auto Entries() const
      -> std::span<const ErasedMethodEntry> {
    return {data, size};
  }
};

struct ObjectDefinition;

// Which storage a property access reaches, on any object of the class the
// access names: the class of the lineage that declares the property, and the
// position that class gave it among its own. A class extending another carries
// its base's properties as well as its own and may declare one of the same
// name, so the pair is the whole answer and what the object turns out to be is
// not consulted (LRM 8.14).
struct PropertyCoordinate {
  const ObjectDefinition* declared_by = nullptr;
  std::uint32_t slot = 0;

  constexpr PropertyCoordinate() = default;
  constexpr PropertyCoordinate(
      const ObjectDefinition* declared_by, std::uint32_t slot)
      : declared_by(declared_by), slot(slot) {
  }
};

// Which dispatch position a call names: the class that introduced the behavior,
// which is the one identity every class answering it agrees on (LRM 8.20), and
// the behavior's ordinal among that class's own introductions. Which body fills
// the position is the object's to answer and is not part of this.
struct BehaviorCoordinate {
  const ObjectDefinition* introduced_by = nullptr;
  std::uint32_t ordinal = 0;

  constexpr BehaviorCoordinate() = default;
  constexpr BehaviorCoordinate(
      const ObjectDefinition* introduced_by, std::uint32_t ordinal)
      : introduced_by(introduced_by), ordinal(ordinal) {
  }
};

// One name a class answers while a reference reaching it resolves, and where
// that name lands. The coordinate sits inside the entry rather than beside it,
// so what a resolution hands back is the address of the entry's own pair: the
// table lives as long as the class does, which is as long as any reference
// settled against it, so nothing has to be copied anywhere to outlive the
// lookup.
struct ResolvedProperty {
  AbiStringRef name;
  PropertyCoordinate at;

  constexpr ResolvedProperty() = default;
  constexpr ResolvedProperty(AbiStringRef name, PropertyCoordinate at)
      : name(name), at(at) {
  }
};

struct ResolvedBehavior {
  AbiStringRef name;
  BehaviorCoordinate at;

  constexpr ResolvedBehavior() = default;
  constexpr ResolvedBehavior(AbiStringRef name, BehaviorCoordinate at)
      : name(name), at(at) {
  }
};

// A set of names a class answers, crossing the generated-runtime boundary as
// plain data. Each is consulted while a reference resolves and never on the
// simulation path: what a name answers with is settled once and applied at each
// access thereafter, whether the answer is a position or a body.
struct ResolvedPropertyTable {
  const ResolvedProperty* data = nullptr;
  std::uint32_t size = 0;

  constexpr ResolvedPropertyTable() = default;
  constexpr ResolvedPropertyTable(
      const ResolvedProperty* data, std::uint32_t size)
      : data(data), size(size) {
  }

  [[nodiscard]] constexpr auto Entries() const
      -> std::span<const ResolvedProperty> {
    return {data, size};
  }
};

struct ResolvedBehaviorTable {
  const ResolvedBehavior* data = nullptr;
  std::uint32_t size = 0;

  constexpr ResolvedBehaviorTable() = default;
  constexpr ResolvedBehaviorTable(
      const ResolvedBehavior* data, std::uint32_t size)
      : data(data), size(size) {
  }

  [[nodiscard]] constexpr auto Entries() const
      -> std::span<const ResolvedBehavior> {
    return {data, size};
  }
};

// One body a class declares under a name. What the class the access names
// declares is what runs, whatever the object turns out to be (LRM 8.14), so
// nothing about the call is left for the object to answer and the answer is the
// address itself rather than a position to look one up at. A method answering a
// dispatch position is not here: for one of those the object decides, and what
// a referrer needs is the coordinate beside this.
struct DeclaredBody {
  AbiStringRef name;
  ErasedMethodEntry body = nullptr;

  constexpr DeclaredBody() = default;
  constexpr DeclaredBody(AbiStringRef name, ErasedMethodEntry body)
      : name(name), body(body) {
  }
};

struct DeclaredBodyTable {
  const DeclaredBody* data = nullptr;
  std::uint32_t size = 0;

  constexpr DeclaredBodyTable() = default;
  constexpr DeclaredBodyTable(const DeclaredBody* data, std::uint32_t size)
      : data(data), size(size) {
  }

  [[nodiscard]] constexpr auto Entries() const
      -> std::span<const DeclaredBody> {
    return {data, size};
  }
};

// Where one property of a class lives on an object of that class, answered by
// whoever laid the object out. It is the whole of what a target has to supply
// about reaching a property: everything else -- which class declares it, how an
// object of a class extending that one gets to it -- is the same question
// whatever the answer is spelled in.
using PropertySlotEntry = void* (*)(void* self);

// The properties one class declares, in the order it gave them positions.
struct PropertySlotTable {
  const PropertySlotEntry* data = nullptr;
  std::uint32_t size = 0;

  constexpr PropertySlotTable() = default;
  constexpr PropertySlotTable(const PropertySlotEntry* data, std::uint32_t size)
      : data(data), size(size) {
  }

  [[nodiscard]] constexpr auto Entries() const
      -> std::span<const PropertySlotEntry> {
    return {data, size};
  }
};

// The same object seen as the class this one extends (LRM 8.13). Only the side
// that laid the object out can say what that is, which is why it is an entry
// rather than an offset: a subobject's position is a property of the target's
// own rules, and here one of those targets is another language's compiler.
using BaseView = void* (*)(void* self);

// How an object of one class answers where the property `declared_by` gave
// `slot` to lives on it. The class the access names is passed rather than
// resolved into a position first, because turning the pair into a position
// takes knowing how the object was laid out -- which is the one thing the
// asking side never has.
using PropertyAccessor = void* (*)(const GcObject* object,
                                   const ObjectDefinition* declared_by,
                                   std::uint32_t slot);

// Which body an object answers `introduced_by`'s `ordinal`-th behavior with
// (LRM 8.20, 8.22). Nothing about the object beyond its class takes part, every
// object of one class answering alike, but which class it is of is the object's
// own to say.
using BehaviorAccessor = ErasedMethodEntry (*)(
    const GcObject* object, const ObjectDefinition* introduced_by,
    std::uint32_t ordinal);

// The two answers a class gives when nothing has laid its objects out flat:
// each walks what the class extends, taking the object's own class as the
// starting point and the entries above as the only things it asks the target
// for. Every class answers this way unless its realization installs something
// that reads a flat schema instead.
[[nodiscard]] auto LineagePropertyAt(
    const GcObject* object, const ObjectDefinition* declared_by,
    std::uint32_t slot) -> void*;
[[nodiscard]] auto LineageBehaviorAt(
    const GcObject* object, const ObjectDefinition* introduced_by,
    std::uint32_t ordinal) -> ErasedMethodEntry;

// The definition of one class: what it extends, what it declares itself, the
// entries by which an object of it is reached, and whatever a realization that
// lays its objects out flat adds. A class joins no lifecycle and holds no place
// in the object tree, and which body initializes an object is settled where the
// object is asked for rather than by the class it is of (LRM 8.7), so what a
// definition carries is what every object of the class shares and nothing about
// any one of them.
//
// Two things a definition never states, and both are why it answers through
// entries. Where a property sits in an object is the answer of whoever laid
// that object out, which for one target is another language's compiler. And
// what a class's lineage adds up to is a fact no unit can see whole, since a
// base may be declared past the boundary -- so a class states its own
// contribution and the answers walk.
//
// The name tables serve a referrer that has no name for the class at all and so
// cannot count a position for itself; they are read while a reference resolves
// and never on the simulation path.
// One behavior a class takes over from its lineage (LRM 8.20): the behavior,
// named the way every reader of one names it, and the body this class answers
// it with.
struct DispatchTakeover {
  const ObjectDefinition* introduced_by = nullptr;
  std::uint32_t ordinal = 0;
  ErasedMethodEntry body = nullptr;

  constexpr DispatchTakeover() = default;
  constexpr DispatchTakeover(
      const ObjectDefinition* introduced_by, std::uint32_t ordinal,
      ErasedMethodEntry body)
      : introduced_by(introduced_by), ordinal(ordinal), body(body) {
  }
};

// The behaviors one class takes over, in no order anyone reads: a takeover
// names the position it answers, so it is found by what it names.
struct TakeoverTable {
  const DispatchTakeover* data = nullptr;
  std::uint32_t size = 0;

  constexpr TakeoverTable() = default;
  constexpr TakeoverTable(const DispatchTakeover* data, std::uint32_t size)
      : data(data), size(size) {
  }

  [[nodiscard]] constexpr auto Entries() const
      -> std::span<const DispatchTakeover> {
    return {data, size};
  }
};

struct ObjectDefinition {
  // The class this one extends (LRM 8.13), or nothing where it extends none.
  // A class states what it adds and nothing about its lineage, so anything it
  // does not declare itself is found by asking what it extends.
  const ObjectDefinition* base = nullptr;
  // What this class declares itself, each in the order it gave positions.
  PropertySlotTable property_slots;
  MethodDispatchTable introductions;
  TakeoverTable takeovers;
  ResolvedPropertyTable property_names;
  ResolvedBehaviorTable behavior_names;
  DeclaredBodyTable body_names;
  BaseView to_base = nullptr;
  // What a realization that lays every object of the class out flat adds, and
  // what only the answers installed by such a realization read.
  MemberStorageSchema members;
  MethodDispatchTable methods;
  std::uint32_t first_member = 0;
  std::uint32_t first_behavior = 0;
  // What this class answers the two questions above with. A class answers by
  // walking its lineage unless its realization installs something that reads
  // the flat schema instead, so every class has an answer and none is null.
  PropertyAccessor property_at = &LineagePropertyAt;
  BehaviorAccessor behavior_at = &LineageBehaviorAt;

  constexpr ObjectDefinition() = default;

  // What a target that lays every object of the class out itself supplies, and
  // the whole of it: the flat schema beside these belongs to a realization that
  // owns the storage, and a class built through this constructor has none. The
  // two answers stay at their defaults, because walking is what a class with no
  // flat schema is answered by.
  constexpr ObjectDefinition(
      const ObjectDefinition* base, PropertySlotTable property_slots,
      MethodDispatchTable introductions, TakeoverTable takeovers,
      ResolvedPropertyTable property_names,
      ResolvedBehaviorTable behavior_names, DeclaredBodyTable body_names,
      BaseView to_base)
      : base(base),
        property_slots(property_slots),
        introductions(introductions),
        takeovers(takeovers),
        property_names(property_names),
        behavior_names(behavior_names),
        body_names(body_names),
        to_base(to_base) {
  }
};

// One name a class declares, and the position it gave that declaration among
// its own. What declares it is left unsaid: a contribution is one class's own,
// so realizing it is what supplies the declarer.
struct DeclaredName {
  AbiStringRef name;
  std::uint32_t position = 0;
};

// What one class adds to its lineage: the class it extends, the storage its own
// properties need, the behaviors it introduces in the order it introduces them,
// the ones it takes over, and the names its own properties and introductions
// answer to. A class states what it adds and nothing about the lineage, which
// is what keeps one declaration's meaning independent of what extends it and
// what lets a class be stated by a unit that cannot see past its own boundary.
// A behavior taken over answers under the name its introducer already gave it,
// so only an introduction brings a name.
struct ClassContribution {
  const ObjectDefinition* base = nullptr;
  std::span<const MemberStorageDescriptor> members;
  std::span<const ErasedMethodEntry> introductions;
  std::span<const DispatchTakeover> takeovers;
  std::span<const DeclaredName> property_names;
  std::span<const DeclaredName> behavior_names;
  std::span<const DeclaredBody> body_names;
};

// The flat forms every value of one class shares. Held apart from the
// definition because a definition names storage rather than owning it, the way
// a scope's and a closure's do, and it must outlive every value built from it.
struct RealizedClass {
  std::vector<MemberStorageDescriptor> members;
  std::vector<ErasedMethodEntry> methods;
  std::vector<ResolvedProperty> property_names;
  std::vector<ResolvedBehavior> behavior_names;
  std::vector<DeclaredBody> body_names;
};

// Completes `definition` by extending what `adds.base` was realized with, into
// storage `realization` holds. A class is realized after the one it extends,
// which is what makes this one step rather than a walk, and a class extending
// nothing starts from empty by taking that step over no base at all.
void RealizeClass(
    const ClassContribution& adds, RealizedClass& realization,
    ObjectDefinition& definition);

// Where `name` lands on `cls`, for a referrer that has no name for the class
// and so could count no position for itself. Both run while a reference
// resolves and neither is reached from the simulation path.
//
// A name the class does not answer throws, because what reaches here was
// resolved against the class's declaration before anything was emitted for it,
// so absence is not a state a legal program reaches -- and answering with
// nothing would put the failure at whatever applied the coordinate, which names
// neither the class nor what was asked of it.
[[nodiscard]] auto FindProperty(
    const ObjectDefinition* cls, std::string_view name)
    -> const PropertyCoordinate*;
[[nodiscard]] auto FindBehavior(
    const ObjectDefinition* cls, std::string_view name)
    -> const BehaviorCoordinate*;

// The body `cls` answers `name` with, for a call that leaves the object nothing
// to decide (LRM 8.14). It answers with the address rather than a coordinate
// because there is no position to count and nothing to count it against: the
// class the access names is the whole of the question, so the walk ends where a
// coordinate walk would only have begun.
[[nodiscard]] auto FindBehaviorBody(
    const ObjectDefinition* cls, std::string_view name) -> ErasedMethodEntry;

// Applying a coordinate to whichever object a handle holds: the object answers
// with its own class, and the class answers the question. Reaching through a
// handle naming no object is the design's own failure (LRM 8.4), the same
// failure as reaching a member by name through one.
//
// A target that carries the view beside the handle and one that carries the
// handle alone each reach this with what they have, which is why the reference
// form is named rather than composed at a call site: what the operation takes
// is the object, and both forms hold it.

// The object a handle names. A handle refers to an object and is not one, and
// where the object's own address sits is not something a handle states, so
// recovering it is this side's answer -- which is what a call entering a body
// settled against a class it cannot name asks for, every body running on the
// object rather than on a reference to it.
[[nodiscard]] auto ObjectOf(const value::ManagedRef& handle) -> void*;

[[nodiscard]] inline auto ObjectOf(const value::ObjectRef& ref) -> void* {
  return ObjectOf(ref.Handle());
}

[[nodiscard]] auto PropertyAt(
    const GcObject* object, const PropertyCoordinate* at) -> void*;
[[nodiscard]] auto BehaviorAt(
    const GcObject* object, const BehaviorCoordinate* at) -> ErasedMethodEntry;

[[nodiscard]] inline auto PropertyAt(
    const value::ManagedRef& handle, const PropertyCoordinate* at) -> void* {
  return PropertyAt(static_cast<const GcObject*>(handle.Share().get()), at);
}

[[nodiscard]] inline auto BehaviorAt(
    const value::ManagedRef& handle, const BehaviorCoordinate* at)
    -> ErasedMethodEntry {
  return BehaviorAt(static_cast<const GcObject*>(handle.Share().get()), at);
}

[[nodiscard]] inline auto PropertyAt(
    const value::ObjectRef& ref, const PropertyCoordinate* at) -> void* {
  return PropertyAt(ref.Handle(), at);
}

[[nodiscard]] inline auto BehaviorAt(
    const value::ObjectRef& ref, const BehaviorCoordinate* at)
    -> ErasedMethodEntry {
  return BehaviorAt(ref.Handle(), at);
}

// A block of storage over a definition: one storage object per member, so a
// member place resolves to that storage's address exactly as a scope member's
// does. What class it is of it adopts like any other object, so that is a
// question about the object rather than about this realization. Two things are
// one of these and they differ only in who ends one -- an object the program
// built with `new`, whose lifetime the simulator owns rather than any scope
// (LRM 8.3), and the storage a block promoted out of its frame, which ends with
// the last hold on it (LRM 6.21).
class ManagedObject : public GcObject {
 public:
  explicit ManagedObject(const ObjectDefinition* definition);

  // Where the property `declared_by` gave `slot` to lives on this object, which
  // is what a place naming it resolves to. The pair is the whole coordinate: a
  // class extending another carries its base's properties as well as its own
  // and may declare one of the same name, so which storage is meant is fixed by
  // the class the access names (LRM 8.14) rather than by what this object is.
  [[nodiscard]] auto MemberAddress(
      const ObjectDefinition* declared_by, std::uint32_t slot) -> void*;

 private:
  StorageBlock members_;
};

}  // namespace lyra::runtime
