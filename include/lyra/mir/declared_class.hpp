#pragma once

#include <string>
#include <variant>

#include "lyra/mir/class_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::mir {

// A class this compilation unit declares, named by its unit-wide identity; the
// unit's class registry resolves the id to the declaration.
struct IntraUnitClassRef {
  ClassId class_id;

  auto operator==(const IntraUnitClassRef&) const -> bool = default;
};

// A class another compilation unit declares, named by the declaring unit and
// the class's path in it. The pair is the identity; how it is spelled belongs
// to whichever target a backend emits, so nothing here composes one.
struct CrossUnitClassRef {
  std::string unit_name;
  support::DefPath class_path;

  auto operator==(const CrossUnitClassRef&) const -> bool = default;
};

// A class some compilation unit declares, as this unit names it. Which unit
// declares a class is part of its identity, as the crate is part of a Rust
// definition's, and a class of this unit is always named by its id however the
// name reaching it was written -- through another instance of this unit, or a
// signature naming this unit -- so one class has one identity here. What is
// known of a class follows from the same fact: this unit holds the definition
// of its own, and of another unit's only what that unit published.
using DeclaredClassRef = std::variant<IntraUnitClassRef, CrossUnitClassRef>;

}  // namespace lyra::mir
