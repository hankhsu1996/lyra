#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct ParamId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ParamId&) const -> std::strong_ordering = default;
};

// It carries no name. Every one of these is a parameter the construction
// protocol takes rather than one the source wrote -- a class the source
// declared states its constructor's formals as ordinary locals of the body --
// so there is no identifier for it to be called by, and a backend spells it
// over the position it sits at.
struct ParamDecl {
  TypeId type;
};

}  // namespace lyra::mir
