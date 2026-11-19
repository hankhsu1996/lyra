#include "lyra/support/value_step.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto ValueStepName(ValueStep step) -> std::string_view {
  switch (step) {
    case ValueStep::kCopy:
      return "copy";
    case ValueStep::kRelocate:
      return "relocate";
    case ValueStep::kAssign:
      return "assign";
    case ValueStep::kEnd:
      return "end";
  }
  throw InternalError("support: unknown value step");
}

}  // namespace lyra::support
