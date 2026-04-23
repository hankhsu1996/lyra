#pragma once

#include <string_view>

#include "lyra/support/internal_error.hpp"

namespace lyra::support {

[[noreturn]] inline auto Unsupported(std::string_view message) -> void {
  throw InternalError(message);
}

}  // namespace lyra::support
