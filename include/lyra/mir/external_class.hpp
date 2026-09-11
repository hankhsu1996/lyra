#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// A class of another unit this one reaches into, as far as that unit published
// it, named by the unit that declares it and its canonical name -- both
// resolved at link time. The two lists below are ordered rather than sets: a
// property's slot and a behavior's ordinal are counted out of them, and those
// properties are a prefix of the class's own storage, so a slot counted here is
// the slot the declaring unit gave.
//
// This unit compiles none of it, which is why it sits apart from the classes
// this unit declares: a walk that emits those cannot reach one, and so cannot
// emit a second definition of a symbol another unit already defines.
struct ExternalClass {
  std::string unit_name;
  std::string class_name;
  // The class it extends, as its own unit promised, named by the same pair and
  // absent where it extends nothing. What it inherited is not listed above:
  // reaching an inherited property is a walk along this chain.
  std::optional<CrossUnitClassRef> base;
  // Whether values of it answer behaviors through a lineage at all. A class
  // commits to an interface rather than extending it, so a behavior an
  // interface class states sits on no lineage and has no position counted
  // through one (LRM 8.26).
  bool is_interface_class = false;
  base::Arena<FieldDecl, FieldId> fields;
  std::vector<std::string> behaviors;
};

// The record kept of the class `class_name` of unit `unit_name`, or nothing
// where this unit consumed no promise about it -- which is the state of every
// class it merely names.
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
