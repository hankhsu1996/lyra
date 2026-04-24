#pragma once

#include <cstdint>

namespace lyra::mir {

// Scoped signal reference that distinguishes module-local vs design-global.
// Allows a single process to legally contain mixed-scope sensitivity while
// keeping MIR clean and future-shareable.
struct SignalRef {
  enum class Scope : uint8_t {
    kModuleLocal,
    kDesignGlobal,
    kObjectLocal,
  };
  Scope scope = Scope::kModuleLocal;
  uint32_t id =
      0;  // body-local slot (kModuleLocal/kObjectLocal) or global slot
  uint32_t object_index = 0;  // owning object index (kObjectLocal only)

  auto operator==(const SignalRef&) const -> bool = default;
};

}  // namespace lyra::mir
