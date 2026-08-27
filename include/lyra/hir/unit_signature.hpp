#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/port_direction.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/type_pool.hpp"

namespace lyra::hir {

struct PublishedMemberId {
  std::uint32_t value;

  auto operator<=>(const PublishedMemberId&) const
      -> std::strong_ordering = default;
};

// One declaration an instance of this unit exposes to another unit by name.
// `net_type` is present when the storage is a net (LRM 6.7), which fixes how
// its drivers resolve and what it holds while undriven, and absent when it is
// a variable -- the pair being what decides the cell a reader reaches. Both
// stand on the publishing unit's own declaration, so a referrer never reads
// that declaration to learn them.
struct PublishedMember {
  std::string name;
  TypeId type;
  std::optional<NetType> net_type;
};

// A port part carrying data across the boundary: which way it flows, the type
// of what crosses, and the member of this unit's instance whose storage it
// reaches. The type is not always the whole of that member: a port expression
// (LRM 23.2.2.2) names part of an internal name, and the two then differ.
// `member` is absent when the port connects to nothing inside the unit, which
// the same clause admits.
struct DataPortPart {
  PortDirection direction{};
  TypeId type{};
  std::optional<PublishedMemberId> member;
};

// A port part naming a scope rather than carrying data -- an interface port
// (LRM 25.3). Nothing flows across it in any direction and no value type
// crosses, so what the unit publishes about one is that it is one.
struct InterfacePortPart {};

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

// The object an instance of this unit is: the class's own name, and the
// members another unit may name on it. A unit's name and the name of the class
// it builds are two facts, so a referrer reads the class it reaches here
// rather than deriving it from the unit it reached through.
struct InstanceClassSignature {
  std::string class_name;
  // In declaration order, which is as much a part of the promise as the names
  // are: a member's position is what fixes where its storage sits, and both
  // sides of the boundary read that position out of this one order.
  base::Arena<PublishedMember, PublishedMemberId> members;

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

}  // namespace lyra::hir
