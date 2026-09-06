#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"

namespace lyra::hir {

// One port identifier of a modport (LRM 25.5). The identifier names an
// expression the interface evaluates -- an item's own name when the modport
// wrote none for it, and whatever the modport wrote otherwise (LRM 25.5.4).
// Reading the name is that expression evaluated and writing it is that
// expression assigned to, so what the interface promises is the pair of
// subroutines carrying the two out, and a referrer reaches the name by calling
// rather than by learning what the expression was.
//
// `setter` is absent where the view admits no write. Which direction the view
// gives the name is not carried beyond that: the front end has already refused
// a use running the wrong way.
//
// `reads` is the storage that expression reaches, which a call cannot show and
// a process waiting on the name has to know: what it observes is every member
// the expression reads, since any of them changing changes what the name
// evaluates to. LRM 25.5 confines those names to the interface's own
// declarations, so each is a member it published.
struct PublishedModportPort {
  std::string name;
  PublishedCallableId getter;
  std::optional<PublishedCallableId> setter;
  std::vector<PublishedMemberId> reads;
};

// A named view of what an interface publishes (LRM 25.5). It narrows which
// names a module written against it may use, and it renames when a port
// identifier carries an expression, so the view itself is part of the promise
// rather than something a referrer derives from the members.
struct PublishedModport {
  std::string name;
  std::vector<PublishedModportPort> ports;

  // The port identifier published under `name`, or nothing when the view
  // offers no such name -- which is the view doing its job, since a name it
  // does not offer is one a module bound through it may not use.
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
