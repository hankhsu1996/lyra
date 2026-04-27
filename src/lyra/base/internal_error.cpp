#include "lyra/base/internal_error.hpp"

#include <string>
#include <utility>

namespace lyra {

InternalError::InternalError(std::string message)
    : std::logic_error(std::move(message)) {
}

}  // namespace lyra
