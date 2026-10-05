#pragma once

#include "lyra/runtime/scope_info.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

// The definition of one class: what it extends, and what a class whose values
// stand in the design hierarchy states of them. Which body initializes a value
// is settled where the value is asked for rather than by the class it is of
// (LRM 8.7), so what a definition carries is what every value of the class
// shares and nothing about any one of them. Which body a value answers a
// behavior with is not among them: the value's class states that in the tables
// its compiled code holds, which this library never reads.
//
// The unit declaring the class emits the definition as one constant, which
// every unit naming the class reaches by the symbol it is linked under; nothing
// is declared to this library while the program runs.
struct ObjectDefinition {
  // The class this one extends (LRM 8.13), or nothing where it extends none.
  const ObjectDefinition* base = nullptr;
  // What only a class whose values stand in the design hierarchy has, absent
  // for every other class.
  const ScopeInfo* scope = nullptr;
};

// A handle owning `object`, a whole value of a class extending the part every
// object starts with, constructed and not yet held by anything: the handle the
// program's `new` answers (LRM 8.3), which ends the value through its virtual
// destructor when the last reference to it goes.
[[nodiscard]] auto AdoptObject(void* object) -> value::ObjectRef;

// The definition of a class an instance of the design hierarchy is built of,
// checked to state what only such a class has: one that states none reaching
// here is a producer that named the wrong class.
[[nodiscard]] auto RequireScopeClass(const ObjectDefinition* definition)
    -> const ObjectDefinition*;

// The part of the object a handle reaches it through: the part belonging to the
// class the handle is of. A handle naming no object is the design's own failure
// (LRM 8.4).
[[nodiscard]] auto ViewOf(const value::ObjectRef& ref) -> void*;

}  // namespace lyra::runtime
