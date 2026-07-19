#pragma once

#include <string>
#include <variant>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/static_property_id.hpp"

namespace lyra::hir {

// A reference to a class declared by this compilation unit. Named by its
// canonical local identity; the unit's class registry resolves the id to the
// declaration.
struct LocalClassRef {
  ClassId class_id;

  auto operator==(const LocalClassRef&) const -> bool = default;
};

// A reference to a class declared by another compilation unit. Named by the
// declaring unit's name and the class's canonical name (the specialization
// name for a parameterized class, the bare name otherwise). The producer (the
// unit naming itself) and every consumer (a referring unit naming a class it
// reaches) compute the same canonical class name from the same bindings by
// the same deterministic function, so a cross-unit reference matches by name
// with no shared table.
struct ExternalClassRef {
  std::string unit_name;
  std::string class_name;

  auto operator==(const ExternalClassRef&) const -> bool = default;
};

// A reference naming a class from a use site: a base's target, a `new`'s
// target, a receiver's declared class, an override's declaring class.
// Intra-unit identity is a compiler-owned id; cross-unit identity is a
// by-name reference resolved against another unit's interface at link time.
// The two never share a key space.
using ClassRef = std::variant<LocalClassRef, ExternalClassRef>;

// A reference to a class property (LRM 8.4) at an access site: the class
// arena the property is declared in, and the slot within that arena. Owner
// is the declaring class, not the receiver's class; the two coincide when
// the receiver's class declares the property itself and diverge when the
// property is inherited from a base.
struct LocalClassPropertyTarget {
  ClassId owner;
  FieldId field;

  auto operator==(const LocalClassPropertyTarget&) const -> bool = default;
};

// A reference to a class property (LRM 8.4) declared by another compilation
// unit: the declaring unit, the class's canonical name, and the property's
// source name. The declaring unit's arena position is not visible here, so
// the property is named directly.
struct ExternalClassPropertyTarget {
  std::string unit_name;
  std::string class_name;
  std::string property_name;

  auto operator==(const ExternalClassPropertyTarget&) const -> bool = default;
};

using ClassPropertyTarget =
    std::variant<LocalClassPropertyTarget, ExternalClassPropertyTarget>;

// A reference to a class static property (LRM 8.9) at an access site: the
// declaring class and the slot within its static-property arena. Owner-
// qualified because the type-associated cell belongs to the declaring class
// even when the source reached it through a derived name.
struct LocalStaticPropertyTarget {
  ClassId owner;
  StaticPropertyId prop;

  auto operator==(const LocalStaticPropertyTarget&) const -> bool = default;
};

// A reference to a class static property (LRM 8.9) declared by another
// compilation unit: the declaring unit, the class's canonical name, and the
// property's source name.
struct ExternalStaticPropertyTarget {
  std::string unit_name;
  std::string class_name;
  std::string property_name;

  auto operator==(const ExternalStaticPropertyTarget&) const -> bool = default;
};

using StaticPropertyTarget =
    std::variant<LocalStaticPropertyTarget, ExternalStaticPropertyTarget>;

// A reference to a class method (LRM 8.6 / 8.10) at an access site: the
// declaring class arena and the method slot within it. The declaring class
// is the owner, not the receiver's class.
struct LocalClassMethodTarget {
  ClassId owner;
  MethodId method;

  auto operator==(const LocalClassMethodTarget&) const -> bool = default;
};

// A reference to a class method (LRM 8.6 / 8.10) declared by another
// compilation unit: the declaring unit, the class's canonical name, and the
// method's source name.
struct ExternalClassMethodTarget {
  std::string unit_name;
  std::string class_name;
  std::string method_name;

  auto operator==(const ExternalClassMethodTarget&) const -> bool = default;
};

using ClassMethodTarget =
    std::variant<LocalClassMethodTarget, ExternalClassMethodTarget>;

}  // namespace lyra::hir
