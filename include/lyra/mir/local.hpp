#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/base/pool_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct LocalId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const LocalId&) const -> std::strong_ordering = default;
};

// Static-lifetime (LRM 6.21) body locals do not live here -- HIR-to-MIR
// realizes each as a cell of whatever its declaration belongs to. A
// pass-by-reference binding (LRM 13.5.2, a `ref` formal or a by-reference
// capture) carries no flag here: its `type` is a `RefType`, so a reference to
// it reaches the place that reference stands for by dereferencing it, the same
// as an observable cell.
struct LocalDecl {
  std::string name;
  TypeId type;
};

}  // namespace lyra::mir
