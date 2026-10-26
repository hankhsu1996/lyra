#pragma once

#include "lyra/runtime/scope_info.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class GcObject;
struct ObjectDefinition;

// What every object starts with: the definition of the class it is an object
// of, written when the object is created whole and never changed after (LRM
// 8.7). Everything the library does to an object whose class it does not know
// -- ending it, driving it through a phase, randomizing it, asking whether it
// is of a class -- it reads here, as a C++ runtime reads a virtual table and a
// Swift one a class's metadata. The root lists it first among its bases, so
// the definition is the object's first word, which is where generated code
// reads it.
struct ObjectHeader {
  const ObjectDefinition* definition = nullptr;
};

// One interface class an object's class implements (LRM 8.26), directly or
// through what it extends, and the table of the bodies answering what that
// interface class declares itself, which only the code calling one reads. An
// interface class's own definition is its identity.
struct InterfaceRow {
  const ObjectDefinition* of = nullptr;
  const void* table = nullptr;
};

// What `randomize()` asks of an object of a source class (LRM 18.6): to
// declare its random variables (LRM 18.4) and state its constraints (LRM 18.5)
// to the call's problem, to name the random objects its handles reach, and the
// hooks before and after (LRM 18.6.2). Each entry is the body the nearest class
// of the object's lineage declaring one gives, which does what its base does
// and then what it adds; none where no class does, which does nothing.
struct ClassObjectInfo {
  void (*rand_variables)(GcObject*, const value::ObjectRef&) = nullptr;
  void (*constrain)(GcObject*, const value::ObjectRef&) = nullptr;
  void (*rand_objects)(GcObject*, const value::ObjectRef&) = nullptr;
  void (*pre_randomize)(GcObject*) = nullptr;
  void (*post_randomize)(GcObject*) = nullptr;
};

// The definition of one class: what it extends, what a class whose values
// stand in the design hierarchy or are objects of a source class states of
// them, how an object of it ends, the interface classes it implements, and the
// body each of its virtual methods runs (LRM 8.20). Which body initializes a
// value is settled where the value is asked for rather than by the class it is
// of (LRM 8.7), so what a definition carries is what every value of the class
// shares and nothing about any one of them.
//
// Every entry is a body entered on an object whose class the caller does not
// know. One driving or randomizing the object is handed it as the root every
// object is and views it as the class whose body it calls; the one ending it
// is handed its storage, which is what the class's destructor takes. The unit
// declaring the class emits the definition as one constant, which every unit
// naming the class reaches by the symbol it is linked under; nothing is
// declared to this library while the program runs.
struct ObjectDefinition {
  // The class this one extends (LRM 8.13), or nothing where it extends none.
  const ObjectDefinition* base = nullptr;
  // What only a class whose values stand in the design hierarchy has, absent
  // for every other class.
  const ScopeInfo* scope = nullptr;
  // What only a source class has, absent for every other class.
  const ClassObjectInfo* class_object = nullptr;
  // Ends an object of the class: every member, the class's own and then what
  // it extends, and then the storage it was created in. Absent for an
  // interface class, of which no object is created.
  void (*end)(void*) = nullptr;
  // Every interface class the class implements, each once however many ways
  // it is reached (LRM 8.26.6.3).
  ConstantSpan<InterfaceRow> interfaces;
  // The table of the body each behavior runs on an object of the class (LRM
  // 8.20), laid out by the code that dispatches on one and read by nothing
  // here. Absent for an interface class.
  const void* methods = nullptr;
};

// Whether an object of the class `object_class` defines is an object of the
// class `cls` defines (LRM 8.16): of it or of one extending it, or of a class
// implementing it where it is an interface class.
[[nodiscard]] auto IsOfClass(
    const ObjectDefinition* object_class, const ObjectDefinition* cls) -> bool;

// The object `ref` names, where it is an object of the class `cls` defines, and
// otherwise no object (LRM 8.16): what a `$cast` toward a class the handle's
// own class does not extend, or toward an interface class, answers.
[[nodiscard]] auto DynamicCast(
    const value::ObjectRef& ref, const ObjectDefinition* cls)
    -> value::ObjectRef;

// The table of the bodies answering what the interface class `iface` declares
// itself, on an object of the class `object_class` defines, which implements
// it (LRM 8.26.5).
[[nodiscard]] auto InterfaceTableOf(
    const ObjectDefinition* object_class, const ObjectDefinition* iface)
    -> const void*;

// Ends `object` through its class's definition.
void EndObject(GcObject* object) noexcept;

// The definition of a class an instance of the design hierarchy is built of,
// checked to state what only such a class has: one that states none reaching
// here is a producer that named the wrong class.
[[nodiscard]] auto RequireScopeClass(const ObjectDefinition* definition)
    -> const ObjectDefinition*;

// The object a handle names, which is where a member is reached whichever class
// the handle is of. A handle naming no object is the design's own failure (LRM
// 8.4).
[[nodiscard]] auto ObjectOf(const value::ObjectRef& ref) -> void*;

}  // namespace lyra::runtime
