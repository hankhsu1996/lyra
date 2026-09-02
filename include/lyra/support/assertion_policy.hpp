#pragma once

#include <cstdint>

namespace lyra::support {

// What lowering does with an assertion construct (LRM 16). Shared vocabulary:
// a design declares it, the command line selects it, and AST-to-HIR acts on it.
// `kSkip` is sound rather than a fidelity trade, because an assertion observes
// and never drives, so a design behaves identically with its assertions elided.
enum class AssertionPolicy : std::uint8_t {
  // Hold the design to its assertions, which today means refusing the forms
  // Lyra does not yet lower.
  kCheck,
  kSkip,
};

}  // namespace lyra::support
