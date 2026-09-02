#include "lyra/support/assertion_policy.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto ElidesAssertions(AssertionPolicy policy) -> bool {
  switch (policy) {
    case AssertionPolicy::kCheck:
      return false;
    case AssertionPolicy::kSkip:
      return true;
  }
  throw InternalError("ElidesAssertions: unknown assertion policy");
}

}  // namespace lyra::support
