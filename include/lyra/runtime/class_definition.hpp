#pragma once

#include "lyra/abi/records.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class GcObject;

// What every object starts with: the definition of the class it is an object
// of, written when the object is created whole and never changed after (LRM
// 8.7). Everything the library does to an object whose class it does not know
// -- ending it, driving it through a phase, randomizing it, asking whether it
// is of a class -- it reads here, as a C++ runtime reads a virtual table and a
// Swift one a class's metadata. The root lists it first among its bases, so
// the definition is the object's first word, which is where generated code
// reads it.
struct ObjectHeader {
  const abi::ObjectDefinition* definition = nullptr;
};

// Whether an object of the class `object_class` defines is an object of the
// class `cls` defines (LRM 8.16): of it or of one extending it, or of a class
// implementing it where it is an interface class.
[[nodiscard]] auto IsOfClass(
    const abi::ObjectDefinition* object_class, const abi::ObjectDefinition* cls)
    -> bool;

// The object `ref` names, where it is an object of the class `cls` defines, and
// otherwise no object (LRM 8.16): what a `$cast` toward a class the handle's
// own class does not extend, or toward an interface class, answers.
[[nodiscard]] auto DynamicCast(
    const value::ObjectRef& ref, const abi::ObjectDefinition* cls)
    -> value::ObjectRef;

// The table of the bodies answering what the interface class `iface` declares
// itself, on an object of the class `object_class` defines, which implements
// it (LRM 8.26.5).
[[nodiscard]] auto InterfaceTableOf(
    const abi::ObjectDefinition* object_class,
    const abi::ObjectDefinition* iface) -> const void*;

// Ends `object` through its class's definition.
void EndObject(GcObject* object) noexcept;

// The definition of a class an instance of the design hierarchy is built of,
// checked to state what only such a class has: one that states none reaching
// here is a producer that named the wrong class.
[[nodiscard]] auto RequireScopeClass(const abi::ObjectDefinition* definition)
    -> const abi::ObjectDefinition*;

// The object a handle names, which is where a member is reached whichever class
// the handle is of. A handle naming no object is the design's own failure (LRM
// 8.4).
[[nodiscard]] auto ObjectOf(const value::ObjectRef& ref) -> void*;

}  // namespace lyra::runtime
