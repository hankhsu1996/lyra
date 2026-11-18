#pragma once

#include "lyra/support/assertion_policy.hpp"
#include "lyra/support/force_targets.hpp"

namespace lyra::support {

// Choices about how to lower that the design does not make for itself: what
// is done with its assertions, and what a force takes over, which the front
// end's reading of the standard decides.
struct LoweringPolicy {
  AssertionPolicy assertions = AssertionPolicy::kCheck;
  ForceTargets force_targets = ForceTargets::kWholeVariables;
};

}  // namespace lyra::support
