#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/hir/callee_interface.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/declared_method.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/random_qualifier.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// One property a class declares (LRM 8.4, 8.9): its name and type, and whether
// it is random (LRM 18.4).
struct DeclaredProperty {
  std::string name;
  TypeId type;
  RandomQualifier random = RandomQualifier::kNone;

  auto operator==(const DeclaredProperty&) const -> bool = default;
};

// The properties of an object of a class, each at the position the class
// places it: the ones another unit may name first, in the order the class
// declares them, then the `local` ones, so adding a `local` property moves no
// position a referrer counted.
using DeclaredProperties = base::Arena<DeclaredProperty, FieldId>;

// The properties of the class itself (LRM 8.9), placed the same way.
using DeclaredStaticProperties =
    base::Arena<DeclaredProperty, StaticPropertyId>;

// The methods a class declares, each at the position the class places it: the
// ones it defines, then its pure virtual prototypes (LRM 8.21), then its
// constraint blocks (LRM 18.5), then the method making each covergroup it
// embeds (LRM 19.4.1), so a position counted here is the method's own.
using DeclaredMethods = base::Arena<DeclaredMethod, MethodId>;

// Where the property named `name` sits among `properties`, or nothing where
// none carries that name. A class declares at most one member under a name.
template <typename Id>
[[nodiscard]] auto FindProperty(
    const base::Arena<DeclaredProperty, Id>& properties, std::string_view name)
    -> std::optional<Id> {
  for (const Id id : properties.Ids()) {
    if (properties.Get(id).name == name) return id;
  }
  return std::nullopt;
}

// The method declared under `name` among `methods`, or nothing where the class
// declares none such. A class declares at most one method under a name.
[[nodiscard]] inline auto FindMethod(
    const DeclaredMethods& methods, std::string_view name)
    -> std::optional<MethodId> {
  for (const MethodId id : methods.Ids()) {
    if (methods.Get(id).prototype.name == name) return id;
  }
  return std::nullopt;
}

// The method among `methods` that `randomize()` calls in place of `hook` on an
// object of the class (LRM 18.6.2), or nothing where the class declares none.
[[nodiscard]] inline auto MethodReplacing(
    const DeclaredMethods& methods, RandomizationHook hook)
    -> std::optional<MethodId> {
  for (const MethodId id : methods.Ids()) {
    if (methods.Get(id).replaces == hook) return id;
  }
  return std::nullopt;
}

// The covergroups a class embeds (LRM 19.4), in the order of the methods
// making them among `methods`.
[[nodiscard]] inline auto EmbeddedCovergroupsOf(const DeclaredMethods& methods)
    -> std::vector<CovergroupId> {
  std::vector<CovergroupId> embedded;
  for (const DeclaredMethod& method : methods) {
    if (method.makes.has_value()) embedded.push_back(*method.makes);
  }
  return embedded;
}

// What a class declares (LRM 8), as the front end elaborated it and as every
// unit asks about it: the one answer to every question about a class, whether
// the unit asking declares it or reaches it in another unit. The unit declaring
// a class computes it once and publishes it as computed (LRM 26.2 puts a
// package's declarations on its signature), so everything another unit's
// compiled output depends on about the class is here and a change to any of it
// is a change to the signature; a unit reaching the class reads it back with
// every identity in it mapped to its own, as rustc's extern providers answer
// the same queries as its local ones, with the same types.
//
// The lists are ordered rather than sets: a property's slot and a virtual
// method's ordinal are counted out of them, so their order is as much a part of
// the signature as their contents.
//
// A class states what it declares and nothing that follows from another class's
// declaration -- no property or method of a class it extends, and no interface
// class it is only by way of one it names -- so a referrer reads each of those
// off the class that declares it.
struct ClassDeclaration {
  support::DefPath class_path;
  // The class this one extends, absent where it extends nothing. A referrer
  // reaches an inherited property or method by walking this chain, reading
  // each class's own declaration.
  std::optional<ClassId> base;
  // Whether this is an interface class (LRM 8.26), which holds no storage and
  // whose methods a class implementing it answers through a part of their own.
  bool is_interface_class = false;
  // The interface classes its declaration names, in the order written (LRM
  // 8.26.2): what a class implements, or what an interface class extends.
  std::vector<ClassId> implements;
  // Every property of an object of the class, as the class places them. A
  // `local` one is named by no other unit (LRM 8.18), but a class of another
  // unit extending this one places its own properties after them all.
  DeclaredProperties properties;
  // Every property of the class itself rather than of an object of it (LRM
  // 8.9), placed the same way. Where a namespace unit holds the class, each is
  // a cell the class keeps, at its position here; where a design element holds
  // it, each is a cell of the instance the class belongs to (LRM 6.22), which
  // the scope replicating the class publishes among its members.
  DeclaredStaticProperties static_properties;
  // What a construction of the class is entered with (LRM 8.7): each formal's
  // direction and type, as a method's prototype states them. Absent for an
  // interface class, of which no object is constructed (LRM 8.26.5).
  std::optional<CalleeInterface> constructor;
  // Every method the class declares, as the class places them.
  DeclaredMethods methods;
  // Whether a construction of the class and a call of a method of the class
  // itself are handed the instance the class belongs to (LRM 6.22), as the
  // class's own declaration states it.
  bool takes_declaring_instance = false;

  auto operator==(const ClassDeclaration&) const -> bool = default;
};

}  // namespace lyra::hir
