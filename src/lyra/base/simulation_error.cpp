#include "lyra/base/simulation_error.hpp"

#include <string>
#include <utility>

namespace lyra {

SimulationError::SimulationError(std::string message)
    : std::runtime_error(std::move(message)) {
}

SimulationError::~SimulationError() = default;

}  // namespace lyra
