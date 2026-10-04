#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/scope_info.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

struct ObjectDefinition;

// Which storage a property access reaches, on any value of the class the
// access names: the class of the lineage that declares the property, and the
// position that class gave it among its own. A class extending another carries
// its base's properties as well as its own and may declare one of the same
// name, so the pair is the whole answer and what the value turns out to be is
// not consulted (LRM 8.14).
struct PropertyCoordinate {
  const ObjectDefinition* declared_by = nullptr;
  std::uint32_t slot = 0;
};

// One property name a class answers while a reference reaching it resolves,
// and where that name lands. The coordinate sits inside the entry rather than
// beside it, so what a resolution hands back is the address of the entry's own
// pair: the table is a constant of the unit declaring the class, so nothing
// has to be copied anywhere to outlive the lookup. A name is that unit's own
// NUL-terminated constant.
struct ResolvedProperty {
  const char* name = nullptr;
  PropertyCoordinate at;
};

// One body a class answers a name with. A call the value gets no say in runs
// what the class the access names declares (LRM 8.14); for one it decides
// (LRM 8.20) the body makes that call on the value. Either way the answer is
// the address itself.
struct DeclaredBody {
  const char* name = nullptr;
  ErasedEntry body = nullptr;
};

// A body of the class declaring one property, answering where that property is
// on a value.
using PropertySlotEntry = void* (*)(void* self);

// The definition of one class: what it extends, the names it declares, and
// where a value of it keeps each property. Which body initializes a value is
// settled where the value is asked for rather than by the class it is of (LRM
// 8.7), so what a definition carries is what every value of the class shares
// and nothing about any one of them. Which body a value answers a behavior with
// is not among them: the value's class states that in the tables its compiled
// code holds, which this library never reads.
//
// Which kind of value carries the class is not one of those facts. A value the
// program built with `new` and an instance standing in the design hierarchy are
// values of a class either way. Where a property lives is asked only of the
// first: a name reaches an instance's storage by walking the tree, so a class
// whose values stand there states no property's position.
//
// Every body a definition names that acts on a value -- one answering a name,
// one answering where a property is -- is entered with the address a handle of
// the class views, the part of the value belonging to that class, and nothing
// on the way converts it. A class extending one other places that one first,
// so its part starts where the value does and its bodies are entered the same
// way on a value of any class extending it. An interface class (LRM 8.26) may
// be placed elsewhere in a value; its bodies make the call the object decides,
// which finds the rest of the value from the part they were entered on.
//
// The name tables serve a referrer that has no name for the class at all.
//
// The unit declaring the class emits the definition as one constant, which
// every unit naming the class reaches by the symbol it is linked under; nothing
// is declared to this library while the program runs.
struct ObjectDefinition {
  // The class this one extends (LRM 8.13), or nothing where it extends none.
  // A class states what it adds and nothing about its lineage, so a name it
  // does not declare itself is found by asking what it extends.
  const ObjectDefinition* base = nullptr;
  ConstantRun<ResolvedProperty> property_names;
  ConstantRun<DeclaredBody> body_names;
  // The body answering where each property this class declares is, in the
  // order the class gave them positions.
  ConstantRun<PropertySlotEntry> property_slots;
  // What only a class whose values stand in the design hierarchy has, absent
  // for every other class.
  const ScopeInfo* scope = nullptr;
};

// A handle owning `object`, a whole value of a class extending the part every
// object starts with, constructed and not yet held by anything: the handle the
// program's `new` answers (LRM 8.3), which ends the value through its virtual
// destructor when the last reference to it goes.
[[nodiscard]] auto AdoptObject(void* object) -> value::ObjectRef;

// The definition, checked before anything is read from it, because a reference
// to a class with no definition is a linkage failure rather than a value.
[[nodiscard]] auto RequireDefinition(const ObjectDefinition* definition)
    -> const ObjectDefinition*;

// The definition of a class an instance of the design hierarchy is built of,
// checked to state what only such a class has: one that states none reaching
// here is a producer that named the wrong class.
[[nodiscard]] auto RequireScopeClass(const ObjectDefinition* definition)
    -> const ObjectDefinition*;

// Where the property `name` lands on `cls`, for a referrer that has no name for
// the class and so could count no position for itself. It runs while a
// reference resolves and is not reached from the simulation path.
//
// A name the class does not answer throws, because what reaches here was
// resolved against the class's declaration before anything was emitted for it,
// so absence is not a state a legal program reaches -- and answering with
// nothing would put the failure at whatever applied the coordinate, which names
// neither the class nor what was asked of it.
[[nodiscard]] auto FindProperty(
    const ObjectDefinition* cls, std::string_view name)
    -> const PropertyCoordinate*;

// The body `cls` answers `name` with: the first class of its lineage declaring
// the name holds it, which is the method the source's call names (LRM 8.14).
// Where the object decides which body runs (LRM 8.20), the body held is one
// making that call on the object. It throws like the lookup above.
[[nodiscard]] auto FindBehaviorBody(
    const ObjectDefinition* cls, std::string_view name) -> ErasedEntry;

// The part of the object a handle reaches it through: the part belonging to the
// class the handle is of, which is what a body that class answers by name runs
// on. A handle naming no object is the design's own failure (LRM 8.4).
[[nodiscard]] auto ViewOf(const value::ObjectRef& ref) -> void*;

// Applying a coordinate to an object: the class declaring the property answers
// where it is. The object is given by whatever reaches it -- the root every
// object shares, a handle, or a write in progress into it. Reaching through a
// handle naming no object is the design's own failure (LRM 8.4), the same
// failure as reaching a member by name through one.
[[nodiscard]] auto PropertyAt(GcObject* object, const PropertyCoordinate* at)
    -> void*;

[[nodiscard]] inline auto PropertyAt(
    const value::ObjectRef& handle, const PropertyCoordinate* at) -> void* {
  return PropertyAt(ObjectRootOf(handle), at);
}

[[nodiscard]] inline auto PropertyAt(
    const ErasedObjectWrite& write, const PropertyCoordinate* at) -> void* {
  return PropertyAt(write.Object(), at);
}

}  // namespace lyra::runtime
