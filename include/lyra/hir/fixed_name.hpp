#pragma once

#include <cstdint>
#include <string>
#include <variant>

namespace lyra::hir {

// The hierarchical name of a scope (LRM 23.6) as far as the text fixes it:
// the package, the classes, the subroutine and the named blocks around it, as
// the front end writes them, which is what a construct inside it reports for
// itself (LRM 21.2.1.5). A package or a class is joined to what it declares by
// `::`, and anything else to what it holds by `.`.
//
// One compiled body serves every instance of a design element, so where the
// scope lies inside an instance its name begins with that instance's, which
// only a run knows, and the text is what follows it -- nothing, for the
// instance's own scope. Elsewhere -- a package, `$unit`, a class declared in
// either -- no instance holds the scope and the text is the whole name.
//
// A body reaches the instance it is replicated with, which for a class is
// the scope declaring it or, for a specialization on a type a scope inside
// that one declares (LRM 8.25), the inner scope; `scopes_out` is how many
// scopes of the design hierarchy the instance the name begins with stands out
// from that one.
struct NameBelowInstance {
  std::string text;
  std::uint32_t scopes_out = 0;

  auto operator==(const NameBelowInstance&) const -> bool = default;
};

struct WholeName {
  std::string text;

  auto operator==(const WholeName&) const -> bool = default;
};

using FixedName = std::variant<NameBelowInstance, WholeName>;

}  // namespace lyra::hir
