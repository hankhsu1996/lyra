#include "lyra/support/port_direction.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto PortDirectionKeyword(PortDirection direction) -> std::string_view {
  switch (direction) {
    case PortDirection::kInput:
      return "input";
    case PortDirection::kOutput:
      return "output";
    case PortDirection::kInOut:
      return "inout";
    case PortDirection::kRef:
      return "ref";
    case PortDirection::kConstRef:
      return "const ref";
  }
  throw InternalError("PortDirectionKeyword: unknown direction");
}

}  // namespace lyra::support
