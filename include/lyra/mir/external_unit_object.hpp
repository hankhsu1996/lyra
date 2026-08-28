#pragma once

#include <string>

#include "lyra/base/arena.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// The object of a unit this one references, as far as that unit published it:
// which unit defines it and the class an instance of it is -- both resolved at
// link time -- and the members it published, at the positions their storage
// sits in. Those members are a prefix of the object, so a position counted here
// is the position the defining unit built. This unit compiles none of it, which
// is why it sits apart from the classes this unit declares: a walk that emits
// those cannot reach one, and so cannot emit a second definition of a symbol
// another unit already defines.
struct ExternalUnitObject {
  std::string unit_name;
  std::string class_name;
  base::Arena<FieldDecl, FieldId> fields;
};

}  // namespace lyra::mir
