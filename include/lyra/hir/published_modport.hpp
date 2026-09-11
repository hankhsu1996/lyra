#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// The storage a name a view defines designates. LRM 25.5.4 sends a modport
// expression to LRM 23.3.3 for what it may be, and a connection there is a
// continuous assignment whose sink is an lvalue -- so a name the view admits a
// write to designates storage rather than computing anything, whatever it is
// spelled as. The parts are that storage: one per declaration the expression
// joins, most significant first, as LRM 23.2.2.1 orders the names a
// concatenation bundles. A name designating one declaration has one part, which
// is this shape with one entry rather than a case of its own.
struct ViewDefinedPlace {
  std::vector<MemberProjection> parts;

  // The type the name itself has, which is the expression's self-determined
  // one (LRM 25.5.4). It is not always the type of the storage underneath: a
  // concatenation is unsigned and as wide as its parts together (LRM 11.4.12),
  // so a view joining even one signed declaration gives the name a type its
  // part does not have, and a referrer that reached only the part would read a
  // different value.
  TypeId type;
};

// The value a name a view offers only for reading stands for. Nothing bounds
// that expression to an lvalue, so it may be a constant, an arithmetic over the
// interface's members, or a designator -- and the one representation covering
// all of them is the subroutine the interface carries the evaluation out in,
// since the expression names declarations that mean nothing where this
// signature is read.
//
// `observes` is what a process waiting on the name waits on: a call shows
// nothing to wait on, so the members the expression reads cross beside it, any
// of them changing being what changes the value. LRM 25.5 confines those names
// to the interface's own declarations, so each is a member it published.
struct ViewComputedValue {
  PublishedCallableId evaluate;
  std::vector<PublishedMemberId> observes;
};

// What one port identifier of a modport means (LRM 25.5). Which of the two it
// is follows from the direction the view declared, which is why the direction
// itself is on no signature: a referrer never asks for it, it asks what the
// name is, and that is this.
using ViewDefinedName = std::variant<ViewDefinedPlace, ViewComputedValue>;

// One port identifier a modport defined for itself (LRM 25.5.4). An identifier
// the view wrote no expression for is absent here: that one is the interface
// item serving twice, already on the member list, and a referrer reaches it
// there exactly as it reaches a name on an unrestricted port.
struct PublishedModportPort {
  std::string name;
  ViewDefinedName meaning;
};

// A named view of what an interface publishes (LRM 25.5). It narrows which
// names a module written against it may use, and it names things of its own, so
// the view itself is part of the promise rather than something a referrer
// derives from the members.
struct PublishedModport {
  std::string name;
  std::vector<PublishedModportPort> ports;

  // The port identifier published under `name`, or nothing when the view
  // defined no such name -- which includes every name it offers under the
  // interface's own spelling, since those are reached as members.
  [[nodiscard]] auto Find(std::string_view name) const
      -> const PublishedModportPort* {
    for (const PublishedModportPort& port : ports) {
      if (port.name == name) return &port;
    }
    return nullptr;
  }
};

// The view published under `name`, or nothing when the unit declares no such
// modport.
[[nodiscard]] inline auto FindModport(
    const std::vector<PublishedModport>& modports, std::string_view name)
    -> const PublishedModport* {
  for (const PublishedModport& modport : modports) {
    if (modport.name == name) return &modport;
  }
  return nullptr;
}

}  // namespace lyra::hir
