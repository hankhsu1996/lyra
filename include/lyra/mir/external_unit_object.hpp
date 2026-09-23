#pragma once

#include <string>

#include "lyra/base/arena.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// The object of a unit this one references, as far as that unit published it:
// which unit defines it and the class an instance of it is -- both resolved at
// link time -- and the members it published, in the order it published them.
// That order is what both sides count, so the position a member sits at here is
// which of the promise's behaviors answers with it; nothing here says where its
// storage sits, because nothing may. This unit compiles none of it, which is
// why it sits apart from the classes this unit declares: a walk that emits
// those cannot reach one, and so cannot emit a second definition of a symbol
// another unit already defines.
struct ExternalUnitObject {
  std::string unit_name;
  std::string class_name;
  base::Arena<PromisedField, FieldId> fields;
};

}  // namespace lyra::mir
