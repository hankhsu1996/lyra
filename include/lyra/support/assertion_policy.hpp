#pragma once

#include <cstdint>

namespace lyra::support {

// What lowering does with an assertion construct (LRM 16). Shared vocabulary:
// a design declares it, the command line selects it, and AST-to-HIR acts on it.
enum class AssertionPolicy : std::uint8_t {
  // Hold the design to its assertions: a form Lyra lowers is checked, and one
  // it does not is refused rather than dropped, so no design is quietly
  // reduced to one that checks nothing.
  kCheck,
  kSkip,
};

// Whether lowering drops an assertion construct instead of translating or
// refusing it -- the one question every consumer asks of the policy, so the
// policy is read here and nowhere else. LRM 16.2: an assertion states what the
// design is required to do and never drives it, so a run with the whole family
// dropped computes the same values, which is what makes dropping them sound
// rather than a fidelity trade.
[[nodiscard]] auto ElidesAssertions(AssertionPolicy policy) -> bool;

}  // namespace lyra::support
