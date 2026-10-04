#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// One behavior a class of another unit introduces, and whether it is pure: one
// that is has no body for a class of this unit extending it to name in its own
// table.
struct PromisedBehavior {
  std::string name;
  bool is_pure = false;
};

// A method of a class of another unit that overrides a behavior one of its
// ancestors introduced (LRM 8.20), and that behavior.
struct PromisedOverride {
  std::string method;
  OverridesExternalSlot behavior;
};

// A class of another unit this one reaches into, as far as that unit published
// it, named by the unit that declares it and its canonical name -- both
// resolved at link time. The lists below are ordered rather than sets: a
// field's slot and a behavior's ordinal are counted out of them, and the named
// fields are a prefix of the class's own storage, so a slot counted here is
// the slot the declaring unit gave.
//
// What a unit promised of its own object is such a class too. It lists no
// fields, because what it published is reached by performing a behavior
// rather than by a slot, and every behavior on it is pure, because a referrer
// holds the object only as what it answers and never a body of it.
//
// This unit compiles none of it, which is why it sits apart from the classes
// this unit declares: a walk that emits those cannot reach one, and so cannot
// emit a second definition of a symbol another unit already defines.
struct ExternalClass {
  std::string unit_name;
  std::string class_name;
  // What its storage is placed after: the class it extends, or the root it
  // extends in the library. Absent for an interface class, which holds no
  // storage (LRM 8.26).
  std::optional<ClassRef> base;
  // Whether values of it answer behaviors through a lineage at all. A class
  // commits to an interface rather than extending it, so a behavior an
  // interface class states sits on no lineage and has no position counted
  // through one (LRM 8.26).
  bool is_interface_class = false;
  // The interfaces its declaration names, in the order written.
  std::vector<CrossUnitClassRef> implements;
  base::Arena<PromisedField, FieldId> fields;
  // The types of the fields no other unit may name, placed after `fields`.
  // Nothing here reaches one; a class extending it places its own after them.
  std::vector<TypeId> private_field_types;
  std::vector<PromisedBehavior> behaviors;
  std::vector<PromisedOverride> overrides;
};

// The record kept of the class `class_name` of unit `unit_name`, or nothing
// where this unit holds no promise about it -- a class no signature the design
// compiles carries.
[[nodiscard]] inline auto FindExternalClass(
    std::span<const ExternalClass> records, std::string_view unit_name,
    std::string_view class_name) -> const ExternalClass* {
  for (const ExternalClass& record : records) {
    if (record.unit_name == unit_name && record.class_name == class_name) {
      return &record;
    }
  }
  return nullptr;
}

}  // namespace lyra::mir
