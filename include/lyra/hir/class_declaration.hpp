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

// One variable the class itself keeps rather than an object of it: a static
// property (LRM 8.9), or a static variable a method or the constructor
// declares (LRM 6.21), each named as the class names it -- a property with no
// scope between, a subroutine's static through the subroutine and every block
// between.
struct DeclaredStatic {
  support::QualifiedName name;
  TypeId type;
  RandomQualifier random = RandomQualifier::kNone;

  auto operator==(const DeclaredStatic&) const -> bool = default;
};

// What the class itself keeps (LRM 8.9, 6.21): its static properties, placed
// as its other properties are, then the static variables of its methods and
// its constructor, subroutine by subroutine in the order the class places its
// methods, the constructor last, and each one's in the order it declares them.
using DeclaredStaticProperties = base::Arena<DeclaredStatic, StaticPropertyId>;

// The methods a class declares, each at the position the class places it: the
// ones it defines, then its pure virtual prototypes (LRM 8.21), then its
// constraint blocks (LRM 18.5), then the method making each covergroup it
// embeds (LRM 19.4.1), so a position counted here is the method's own.
using DeclaredMethods = base::Arena<DeclaredMethod, MethodId>;

// Where the property named `name` sits among `properties`, or nothing where
// none carries that name. A class declares at most one member under a name.
[[nodiscard]] inline auto FindProperty(
    const DeclaredProperties& properties, std::string_view name)
    -> std::optional<FieldId> {
  for (const FieldId id : properties.Ids()) {
    if (properties.Get(id).name == name) return id;
  }
  return std::nullopt;
}

// Where the class keeps what it names `name` among `statics`, or nothing where
// it keeps nothing under that name.
[[nodiscard]] inline auto FindStatic(
    const DeclaredStaticProperties& statics, const support::QualifiedName& name)
    -> std::optional<StaticPropertyId> {
  for (const StaticPropertyId id : statics.Ids()) {
    if (statics.Get(id).name == name) return id;
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

// One behavior of an interface class a value of a class is also a value of
// (LRM 8.26.5), and the behavior of the class's own lineage answering it, as
// the front end resolved its name there (LRM 8.26.2). Nothing answers it where
// the class is abstract and leaves the implementation to a class extending it.
struct ConformingBehavior {
  ClassMethodTarget interface_behavior;
  std::optional<ClassMethodTarget> answered_by;

  auto operator==(const ConformingBehavior&) const -> bool = default;
};

// What a construction of a class is entered with (LRM 8.7): each formal's
// direction and type, as a method's prototype states them, and at the position
// of each formal that has a default, the method of the class evaluating it
// (LRM 13.5.3).
struct DeclaredConstructor {
  CalleeInterface interface;
  std::vector<std::optional<MethodId>> defaults;

  auto operator==(const DeclaredConstructor&) const -> bool = default;
};

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
// off the class that declares it. The one exception is what answers each
// behavior of the interface classes a value of it is also a value of, which the
// front end resolves along the whole lineage (LRM 8.26.2) and a referrer reads
// rather than resolves again.
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
  // Everything the class itself keeps rather than an object of it (LRM 8.9,
  // 6.21). Where a namespace unit holds the class, each is a cell the class
  // keeps, at its position here; where a design element holds it, each is a
  // cell of the instance the class belongs to (LRM 6.22), which the scope
  // replicating the class publishes among its members. A method's static is
  // the class's in the same way, declared here and nowhere in the method's
  // body, so every body and every unit naming it names this entry.
  DeclaredStaticProperties static_properties;
  // What a construction of the class is entered with (LRM 8.7). Absent for an
  // interface class, of which no object is constructed (LRM 8.26.5).
  std::optional<DeclaredConstructor> constructor;
  // Every method the class declares, as the class places them.
  DeclaredMethods methods;
  // The scope of the design hierarchy whose instance the class belongs to
  // (LRM 6.22): the one declaring it, or for a specialization of a generic
  // class on a type of such a scope, that scope (LRM 8.25). A construction of
  // the class and a call of a method of the class itself are handed that
  // instance, and its code is the code of the body realizing the scope. An
  // interface class belongs to one as any class does, and has neither a
  // construction nor code (LRM 8.26). Absent where the class belongs to no
  // instance.
  std::optional<support::DefPath> instance_scope;
  // Which behavior answers each behavior of every interface class a value of
  // the class is also a value of; none for an interface class.
  std::vector<ConformingBehavior> conforming;

  auto operator==(const ClassDeclaration&) const -> bool = default;
};

}  // namespace lyra::hir
