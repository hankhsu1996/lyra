#pragma once

#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/class_value.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

// An object the program built with `new` (LRM 8.3), whose lifetime the
// simulator owns rather than any scope. It is a value of a class and adds no
// state to one: what makes it this kind is how it is allocated and how it is
// reached, neither of which is anything it holds. It is a type of its own so
// that the allocation names what it is making, and so that what reclamation
// comes to need has somewhere to land.
//
// Its members follow it in the allocation that makes it, so only that
// allocation builds one.
class ManagedObject : public ClassValue {
 private:
  friend class ClassValue;

  explicit ManagedObject(const ObjectDefinition* definition)
      : ClassValue(definition, sizeof(ManagedObject)) {
  }
};

// An object of class `definition`, and the reference that owns it.
[[nodiscard]] auto MakeManagedObject(const ObjectDefinition* definition)
    -> value::ObjectRef;

}  // namespace lyra::runtime
