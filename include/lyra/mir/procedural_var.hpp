#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct ProceduralVarId {
  std::uint32_t value;

  auto operator<=>(const ProceduralVarId&) const
      -> std::strong_ordering = default;
};

// LRM 13.5.2 pass-by-reference. A kValue var owns its storage; a kReference var
// (a `ref` / `const ref` formal) is an alias to the actual's cell, so the
// backend renders it as a `Ref<T>` and reads / writes route through the
// actual's own access path. The backend distinguishes the two read / write
// renderings on this axis.
enum class VariableBinding : std::uint8_t {
  kValue,
  kReference,
};

// Static-lifetime (LRM 13.3.1) body locals do not live here -- HIR-to-MIR
// realizes them as structural vars on the enclosing structural scope
// (`docs/decisions/variable-lifetime-storage.md`).
struct ProceduralVarDecl {
  std::string name;
  TypeId type;
  VariableBinding binding = VariableBinding::kValue;
};

}  // namespace lyra::mir
