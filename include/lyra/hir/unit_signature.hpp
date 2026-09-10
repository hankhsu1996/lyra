#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/external_class.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/port_direction.hpp"
#include "lyra/hir/published_behavior.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

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
// declarations on its signature): its canonical name, the properties it
// declares in the order that fixes their slots, and the behaviors it introduces
// in the order that fixes their ordinals.
//
// A class states what it adds and nothing about the lineage it extends, so a
// referrer counts a position out of the class that declares it and never
// through a lineage it cannot see. What a class keeps to itself (LRM 8.18
// `local`) is absent, and the class places what it publishes ahead of it, so
// adding one moves nothing a referrer counted.
struct ClassSignature {
  std::string class_name;
  // The class this one extends, named the way every class named on a signature
  // is -- by declaring unit and canonical name -- and absent where it extends
  // nothing. A referrer resolves an inherited property or behavior by walking
  // this chain, which is why nothing inherited is restated below: stating it
  // would mean reading the base's promise while deriving this one, and a
  // signature is a function of its own unit's declarations alone.
  std::optional<ExternalClassRef> base;
  // Whether this is an interface class (LRM 8.26). A class commits to one
  // rather than extending it, so a behavior an interface class states sits on
  // no lineage; a referrer that could not tell would name a coordinate no value
  // carries.
  bool is_interface_class = false;
  base::Arena<PublishedMember, PublishedMemberId> members;
  base::Arena<PublishedBehavior, PublishedBehaviorId> behaviors;

  // The property published under `name`, or nothing where the class published
  // no such name -- which is what leaves a reference to it with nothing to
  // compile against.
  [[nodiscard]] auto FindMember(std::string_view name) const
      -> std::optional<PublishedMemberId> {
    for (const PublishedMemberId id : members.Ids()) {
      if (members.Get(id).name == name) return id;
    }
    return std::nullopt;
  }

  // The behavior published under `name`, or nothing where this class
  // introduces none such -- a class that answers a behavior it did not
  // introduce is not where a dispatch names it.
  [[nodiscard]] auto FindBehavior(std::string_view name) const
      -> std::optional<PublishedBehaviorId> {
    for (const PublishedBehaviorId id : behaviors.Ids()) {
      if (behaviors.Get(id).name == name) return id;
    }
    return std::nullopt;
  }
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

  // The class published under `name`, or nothing where the unit published no
  // such name.
  [[nodiscard]] auto FindClass(std::string_view name) const
      -> const ClassSignature* {
    for (const ClassSignature& published : classes) {
      if (published.class_name == name) return &published;
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

}  // namespace lyra::hir
