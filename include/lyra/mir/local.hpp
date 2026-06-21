#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct LocalId {
  std::uint32_t value;

  auto operator<=>(const LocalId&) const -> std::strong_ordering = default;
};

// Static-lifetime (LRM 13.3.1) body locals do not live here -- HIR-to-MIR
// realizes them as members on the enclosing class
// (`docs/decisions/variable-lifetime-storage.md`). A pass-by-reference binding
// (LRM 13.5.2, a `ref` formal or a by-reference capture) carries no flag here:
// its `type` is a `RefType`, and the backend renders reads / writes through
// `Ref<T>` from the type alone.
struct LocalDecl {
  std::string name;
  TypeId type;
};

}  // namespace lyra::mir
