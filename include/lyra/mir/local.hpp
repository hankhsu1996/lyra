#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "lyra/base/pool_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct LocalId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const LocalId&) const -> std::strong_ordering = default;
};

// It carries no name. A body's own storage is reached by the position it sits
// at and by nothing else -- no name outside the body resolves to a local, which
// is what makes a local local -- so an identifier here would serve only how the
// declaration reads. What the source wrote is kept as a relation the body holds
// (`NamedLocal`), for exactly that: a reader of the IR or of emitted text sees
// the name the design used, and a local the lowering added for its own working
// has none to show.
//
// Static-lifetime (LRM 6.21) body locals do not live here -- HIR-to-MIR
// realizes each as a cell of whatever its declaration belongs to. A
// pass-by-reference binding (LRM 13.5.2, a `ref` formal or a by-reference
// capture) carries no flag here: its `type` is a `RefType`, so a reference to
// it reaches the place that reference stands for by dereferencing it, the same
// as an observable cell.
struct LocalDecl {
  TypeId type;
};

// One entry of the relation between a body and the identifiers its source
// declared: the identifier, and the local it stands for.
struct NamedLocal {
  std::string name;
  LocalId local;
};

// The identifier `local` was declared under among `named`, or nothing where the
// lowering introduced it.
[[nodiscard]] inline auto NameOf(
    std::span<const NamedLocal> named, LocalId local)
    -> std::optional<std::string_view> {
  for (const NamedLocal& entry : named) {
    if (entry.local == local) {
      return std::string_view{entry.name};
    }
  }
  return std::nullopt;
}

}  // namespace lyra::mir
