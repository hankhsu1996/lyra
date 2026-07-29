#include "lyra/base/simulation_error.hpp"

#include <string>
#include <utility>

namespace lyra {

SimulationError::SimulationError(std::string message)
    : std::runtime_error("runtime error: " + std::move(message)) {
}

}  // namespace lyra
