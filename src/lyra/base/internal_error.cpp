#include "lyra/base/internal_error.hpp"

#include <string>
#include <utility>

namespace lyra {

InternalError::InternalError(std::string message)
    : std::logic_error(
          "Internal error: " + std::move(message) +
          "\nThis is a bug in Lyra. Please report at: "
          "https://github.com/hankhsu1996/lyra/issues") {
}

}  // namespace lyra
