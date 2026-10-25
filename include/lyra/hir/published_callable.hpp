#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

// Where a published callable sits in the list its unit published. A callable is
// reached by the symbol its declaring unit emits it under rather than by a
// position in an object, so this orders the publication and nothing else.
struct PublishedCallableId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedCallableId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::hir
