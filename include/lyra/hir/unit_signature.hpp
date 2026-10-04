#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/external_class.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/port_direction.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_method.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/type_import.hpp"

namespace lyra::hir {

// A port part carrying data across the boundary: which way it flows, the type
// of what crosses, and what inside this unit's instance it reaches. The type is
// the port expression's own self-determined type (LRM 23.2.2.2), which is
// narrower than the storage behind it whenever that expression names part of an
// internal name.
struct DataPortPart {
  PortDirection direction{};
  TypeId type{};
  ConnectionTarget target;
};

// A port part naming a scope rather than carrying data -- an interface port
// (LRM 25.3). Nothing flows across it in any direction, so it has no direction
// and no value type; what crosses is which member of this unit's instance the
// connection binds, and the type of that member says which unit's objects it
// must be bound to and how many of them.
struct InterfacePortPart {
  PublishedMemberId member;
};

// One point of a port that a connection reaches individually. A port bundling
// several internal names (LRM 23.2.2.1) carries data separately across each,
// under directions and types that need not agree, so it has one part per
// bundled name; every other port has exactly one, which is the same shape with
// one entry.
using PortPart = std::variant<DataPortPart, InterfacePortPart>;

// One port a unit publishes (LRM 23.2.2). `name` is the external name -- what
// another unit connects to, which the LRM lets differ from the name of
// whatever the port reaches inside the unit, so what is published is the port
// and never the declaration behind it.
struct PortDecl {
  std::string name;
  // Least significant first, since LRM 23.2.2.1 gives the first bundled name
  // written the most significant bits and a connection reaches them in bit
  // order.
  std::vector<PortPart> parts;
  // What an instance leaving the port unconnected receives (LRM 23.2.2.4). The
  // default is an expression of this unit, evaluated in its own scope and not
  // in the instantiator's, so what crosses is the subroutine that evaluates it.
  // Only an input port declared in the header may have one.
  std::optional<PublishedCallableId> default_value;
};

// The object an instance of this unit is: the class's own name, the members
// another unit may name on it, the subroutines another unit may call on it,
// and the views it offers over those members. A unit's name and the name of
// the class it builds are two facts, so a referrer reads the class it reaches
// here rather than deriving it from the unit it reached through.
struct InstanceClassSignature {
  std::string class_name;
  // The order is as much a part of the promise as the names are: a member's
  // position is what fixes where its storage sits, and both sides of the
  // boundary read that position out of this one order.
  base::Arena<PublishedMember, PublishedMemberId> members;
  base::Arena<PublishedCallable, PublishedCallableId> callables;
  std::vector<PublishedModport> modports;

  // The member published under `name`, or nothing when the unit published no
  // such name. A name with no answer here is one the unit never promised, and
  // that is exactly what leaves a reference to it resolving at elaboration.
  [[nodiscard]] auto Find(std::string_view name) const
      -> std::optional<PublishedMemberId> {
    for (const PublishedMemberId id : members.Ids()) {
      if (members.Get(id).name == name) return id;
    }
    return std::nullopt;
  }
};

// One class of the source language a unit publishes (LRM 26.2 puts a package's
// declarations on its signature), named by the canonical name a referrer
// reaches it under. It is the class's own declaration as the front end
// elaborated it, so everything another unit's compiled output depends on about
// the class is here and a change to any of it is a change to the signature.
// The lists are ordered rather than sets: a property's slot and a virtual
// method's ordinal are counted out of them, so their order is as much a part of
// the signature as their contents.
//
// A class states what it declares and nothing that follows from another class's
// declaration -- no property or method of a class it extends, and no interface
// class it is only by way of one it names -- so a referrer reads each of those
// off the class that declares it.
struct ClassSignature {
  std::string class_name;
  // The class this one extends, named the way every class named on a signature
  // is -- by declaring unit and canonical name -- and absent where it extends
  // nothing. A referrer reaches an inherited property or method by walking
  // this chain, reading each class's own signature.
  std::optional<ExternalClassRef> base;
  // Whether this is an interface class (LRM 8.26), which holds no storage and
  // whose methods a class implementing it answers through a part of their own.
  bool is_interface_class = false;
  // The interface classes its declaration names, in the order written (LRM
  // 8.26.2): what a class implements, or what an interface class extends.
  std::vector<ExternalClassRef> implements;
  // The properties another unit may name, in the order the class declares them,
  // which the class places at the start of its own storage -- so adding a
  // `local` property moves no slot a referrer counted.
  PublishedProperties properties;
  // The type of every `local` property (LRM 8.18), in the order the class
  // places them after those. None is named, so no referrer reaches one; a class
  // of another unit extending this one still has to know how much storage they
  // take, because its own properties are placed after them.
  std::vector<TypeId> local_property_types;
  // The properties of the class itself rather than of an object of it (LRM
  // 8.9) that another unit may name. Each is one cell the declaring unit holds
  // and a referrer reaches by name, so no position is counted out of this
  // list.
  std::vector<PublishedProperty> static_properties;
  // What a construction of the class is entered with (LRM 8.7): each formal's
  // direction and type, as a method's prototype states them. Absent for an
  // interface class, of which no object is constructed (LRM 8.26.5).
  std::optional<ExternalCalleeInterface> constructor;
  // Every method the class declares, in the order it declares them.
  std::vector<PublishedMethod> methods;
};

// What a unit publishes: the declarations another unit may name. Derived by the
// unit from its own declarations alone, so nothing it states can contradict
// what the unit is, and nothing about any other unit is needed to produce it --
// which is what lets every unit's be derived at once, in any order.
//
// A unit that publishes nothing has an empty signature rather than none.
struct UnitSignature {
  std::string unit_name;
  // The types the published declarations name, held here rather than named in
  // the publishing unit's pool: a signature is read where that unit's arenas
  // are not, so an identity on one has to index storage the signature carries.
  // For the same reason a class named in here is named by declaring unit and
  // class name, never by an id.
  TypePool types;
  // In declaration order, which is the order a positional connection counts
  // through (LRM 23.3.2.1). A consumer walking a unit's connections walks these
  // parts in step with them rather than searching for each, so the two cannot
  // disagree about which point is which.
  std::vector<PortDecl> ports;
  // Absent on a unit with no instance: a package names its declarations and
  // roots no object, so nothing reaches it through a receiver.
  std::optional<InstanceClassSignature> instance_class;
  // The classes of the source language this unit declares and other units may
  // name (LRM 26.2), each reached by its own name rather than through any
  // instance.
  std::vector<ClassSignature> classes;
  // The subroutines this unit declares in its namespace, which another unit
  // calls by name on no object (LRM 26.3): what a call to each passes and
  // awaits. Empty on a design element, whose subroutines are enabled on an
  // instance of it and so are stated on the object that instance is.
  std::vector<PublishedCallable> subroutines;

  // Whether this unit is a design element (LRM 23.2.1), which exists to be
  // instantiated and wired and so publishes its ports and nothing it declares
  // inside. The object its instances are is what says so: a unit that roots one
  // is a design element, and a namespace unit roots none.
  [[nodiscard]] auto IsDesignElement() const -> bool {
    return instance_class.has_value();
  }

  // The class published under `name`, or nothing where the unit published no
  // such name.
  [[nodiscard]] auto FindClass(std::string_view name) const
      -> const ClassSignature* {
    for (const ClassSignature& published : classes) {
      if (published.class_name == name) return &published;
    }
    return nullptr;
  }

  // The namespace subroutine published under `name`, on the same terms.
  [[nodiscard]] auto FindSubroutine(std::string_view name) const
      -> const PublishedCallable* {
    for (const PublishedCallable& published : subroutines) {
      if (published.name == name) return &published;
    }
    return nullptr;
  }
};

// The class an instance of the unit named `unit_name` is. The unit both
// publishes this on its signature and builds the class under it, so the promise
// and the code cannot name different classes. Only the publishing unit computes
// it -- a referrer reads the name the signature carries, which is what keeps a
// unit's name and its class two facts everywhere but here.
[[nodiscard]] inline auto InstanceClassName(std::string_view unit_name)
    -> std::string {
  return std::string{unit_name};
}

// The object an instance of the unit `signature` describes is. A unit whose
// instances exist roots one, so a caller holding the signature of a unit it
// instantiates reaches it without a case for its absence.
[[nodiscard]] inline auto InstanceClassOf(const UnitSignature& signature)
    -> const InstanceClassSignature& {
  if (!signature.instance_class.has_value()) {
    throw InternalError(
        "hir::InstanceClassOf: a unit that is instantiated publishes the "
        "object its instances are");
  }
  return *signature.instance_class;
}

// The record a referrer keeps of the object `signature` promises, with the
// member types taken into `into` -- the referrer's own pool, since an identity
// on a signature indexes storage the signature carries. The whole published
// list crosses, not the part a referrer happens to name: a member's position is
// counted out of that list.
[[nodiscard]] auto ImportExternalUnitObject(
    const UnitSignature& signature, TypePool& into) -> ExternalUnitObject;

// The record a referrer keeps of one class `signature` publishes, on the same
// terms: the whole published list crosses and the types are answered again out
// of the reader's pool.
[[nodiscard]] auto ImportExternalClass(
    const UnitSignature& signature, const ClassSignature& published,
    TypePool& into) -> ExternalClass;

// `interface` and `callable` with every type they name taken into the pool
// `importer` writes, which is the part of each that cannot cross from one pool
// to another as it stands.
[[nodiscard]] auto ImportCalleeInterface(
    TypeImporter& importer, ExternalCalleeInterface interface)
    -> ExternalCalleeInterface;
[[nodiscard]] auto ImportCallable(
    TypeImporter& importer, PublishedCallable callable) -> PublishedCallable;

// A class's properties and methods taken the same way, in the order given.
[[nodiscard]] auto ImportTypes(
    TypeImporter& importer, std::span<const TypeId> types)
    -> std::vector<TypeId>;
[[nodiscard]] auto ImportProperties(
    TypeImporter& importer, const PublishedProperties& properties)
    -> PublishedProperties;
[[nodiscard]] auto ImportStaticProperties(
    TypeImporter& importer, std::span<const PublishedProperty> properties)
    -> std::vector<PublishedProperty>;
[[nodiscard]] auto ImportMethods(
    TypeImporter& importer, std::span<const PublishedMethod> methods)
    -> std::vector<PublishedMethod>;

}  // namespace lyra::hir
