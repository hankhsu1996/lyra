#include "lyra/base/simulation_error.hpp"

#include <utility>

#include "lyra/base/diag_message.hpp"

namespace lyra {

SimulationError::SimulationError(base::DiagMessage message)
    : std::runtime_error(std::move(message).TakeText()) {
}

SimulationError::~SimulationError() = default;

}  // namespace lyra
