#pragma once

#include <compare>
#include <cstddef>
#include <functional>
#include <string>
#include <variant>

#include "lyra/base/hash.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// The name every unit gives one class: the unit declaring it and the class's
// path in that unit. The declaring unit and every unit reaching the class
// compute the same pair from the same declarations by the same deterministic
// function, so it is what a signature writes and what a symbol is spelled from.
// Inside one unit a class is named by its `ClassId` instead, which the unit's
// class table maps to and from this pair.
struct ClassKey {
  std::string unit_name;
  support::DefPath class_path;

  auto operator<=>(const ClassKey&) const -> std::strong_ordering = default;
  // Two equal keys display alike, which is all a pool needs of a hash; equality
  // decides which key is which.
  struct Hash {
    auto operator()(const ClassKey& key) const -> std::size_t {
      std::size_t seed = std::hash<std::string>{}(key.unit_name);
      base::HashField(seed, support::DisplayOf(key.class_path));
      return seed;
    }
  };
};

// A class property (LRM 8.4) at an access site: the class declaring it, and
// which of that class's properties it is. The declaring class is the owner, not
// the receiver's class; the two coincide when the receiver's class declares the
// property itself and diverge when the property is inherited from a base.
struct ClassPropertyTarget {
  ClassId owner;
  FieldId field;

  auto operator==(const ClassPropertyTarget&) const -> bool = default;
};

// A class static property (LRM 8.9) at an access site: the declaring class and
// which of its static properties it is. Owner-qualified because the
// type-associated cell belongs to the declaring class even when the source
// reached it through a derived name.
struct ClassStaticPropertyTarget {
  ClassId owner;
  StaticPropertyId prop;

  auto operator<=>(const ClassStaticPropertyTarget&) const
      -> std::strong_ordering = default;
};

// A class method (LRM 8.6 / 8.10) at an access site: the declaring class and
// which of its methods it is. The declaring class is the owner, not the
// receiver's class.
//
// It also names a behavior (LRM 8.20) -- what a method overrides, what a call
// dispatches on, what an interface class's behavior is and is answered by --
// as the method that introduced it, which is the one identity every class
// overriding it agrees on.
struct ClassMethodTarget {
  ClassId owner;
  MethodId method;

  auto operator==(const ClassMethodTarget&) const -> bool = default;
};

}  // namespace lyra::hir
