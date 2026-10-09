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
#include "lyra/support/def_path.hpp"

namespace lyra::mir {

// One behavior a class of another unit introduces, and whether it is pure: one
// that is has no body for a class of this unit extending it to name in its own
// table.
struct PublishedBehavior {
  std::string name;
  bool is_pure = false;
};

// A method of a class of another unit that overrides a behavior one of its
// ancestors introduced (LRM 8.20), and that behavior.
struct PublishedOverride {
  std::string method;
  OverridesExternalSlot behavior;
};

// A class of another unit this one reaches into, as far as that unit published
// it, named by the unit that declares it and its path in it -- both
// resolved at link time. The lists below are ordered rather than sets: a
// field's slot and a behavior's ordinal are counted out of them, and the named
// fields are a prefix of the class's own storage, so a slot counted here is
// the slot the declaring unit gave.
//
// What a unit published of the class one of its scopes is -- an instance, or
// a generate block inside one -- is such a class too. Its fields are what the
// scope published, in the order it published them, and its published
// subroutines are methods a referrer calls directly; it names no behavior,
// because none of its methods dispatches.
//
// This unit compiles none of it, which is why it sits apart from the classes
// this unit declares: a walk that emits those cannot reach one, and so cannot
// emit a second definition of a symbol another unit already defines.
struct ExternalClass {
  std::string unit_name;
  support::DefPath class_path;
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
  std::vector<DeclaredClassRef> implements;
  // The fields it published, in the order it published them, each counted to
  // the same position by both sides; and which of them answer to an
  // identifier. A scope's published class also holds what a name steps through
  // or ends at without spelling an identifier of its own -- what a generate
  // construct built, what a `disable` of a block ends -- so not every field
  // answers to one.
  base::Arena<FieldDecl, FieldId> fields;
  std::vector<NamedField> named_fields;
  // The types of the fields no other unit may name, placed after `fields`.
  // Nothing here reaches one; a class extending it places its own after them.
  std::vector<TypeId> private_field_types;
  std::vector<PublishedBehavior> behaviors;
  std::vector<PublishedOverride> overrides;
};

// The record kept of the class `class_path` of unit `unit_name`, or nothing
// where this unit holds no published record of it -- a class no signature the
// design compiles carries.
[[nodiscard]] inline auto FindExternalClass(
    std::span<const ExternalClass> records, std::string_view unit_name,
    const support::DefPath& class_path) -> const ExternalClass* {
  for (const ExternalClass& record : records) {
    if (record.unit_name == unit_name && record.class_path == class_path) {
      return &record;
    }
  }
  return nullptr;
}

}  // namespace lyra::mir
