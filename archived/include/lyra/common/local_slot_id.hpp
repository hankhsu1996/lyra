#pragma once

#include <cstdint>
#include <format>

#include "lyra/common/internal_error.hpp"

namespace lyra::mir {
struct PlaceRoot;
}

namespace lyra::common {

// Typed body-local slot identity. Indexes into ModuleBody.slots.
// Must be used instead of raw uint32_t for module-local slot identity
// across all body-local APIs.
struct LocalSlotId {
  uint32_t value = 0;
  auto operator==(const LocalSlotId&) const -> bool = default;
};

}  // namespace lyra::common
