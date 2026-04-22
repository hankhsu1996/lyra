#pragma once

#include <cstdint>

namespace lyra::mir {

// Compile-time identity of a construction program point where a child
// may be created. One site can produce zero, one, or many realized
// descendants at construction time (generate if/for/case).
// Site identity is stable across instances of the same specialization.
struct ChildBindingSiteId {
  uint32_t value = 0;
  auto operator==(const ChildBindingSiteId&) const -> bool = default;
};

}  // namespace lyra::mir
