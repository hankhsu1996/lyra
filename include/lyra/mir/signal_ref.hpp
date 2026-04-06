#pragma once

#include <cstdint>

namespace lyra::mir {

// Scoped signal reference that distinguishes module-local vs design-global.
// Allows a single process to legally contain mixed-scope sensitivity while
// keeping MIR clean and future-shareable.
struct SignalRef {
  enum class Scope : uint8_t { kModuleLocal, kDesignGlobal };
  Scope scope;
  uint32_t
      id;  // body-local index (kModuleLocal) or global slot (kDesignGlobal)

  auto operator==(const SignalRef&) const -> bool = default;
};

}  // namespace lyra::mir
