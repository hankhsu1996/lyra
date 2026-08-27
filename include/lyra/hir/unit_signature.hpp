#pragma once

#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/port_direction.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/type_pool.hpp"

namespace lyra::hir {

// A port part carrying data across the boundary: which way it flows, and the
// type of what crosses. The type is an identity in the signature's own pool, so
// a consumer reads it without reaching into the unit that published it.
struct DataPortPart {
  PortDirection direction;
  TypeId type;
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
// another unit connects to, which the LRM lets differ from the name of whatever
// the port reaches inside the unit, so what is published is the port and never
// the declaration behind it.
struct PortDecl {
  std::string name;
  // Least significant first, since LRM 23.2.2.1 gives the first bundled name
  // written the most significant bits and a connection reaches them in bit
  // order.
  std::vector<PortPart> parts;
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
};

}  // namespace lyra::hir
