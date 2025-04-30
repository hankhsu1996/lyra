#pragma once

#include <string>
#include <vector>

#include "core/simulation_result.hpp"
#include "simulation/simulation_options.hpp"
#include "simulation/simulator.hpp"

namespace lyra {

// Run simulation from SystemVerilog source code string
inline auto RunFromSource(
    const std::string& code, const SimulationOptions& options = {})
    -> SimulationResult {
  return Simulator::RunFromSource(code, options);
}

// Run simulation from a list of source file paths
inline auto RunFromFiles(
    const std::vector<std::string>& paths,
    const SimulationOptions& options = {}) -> SimulationResult {
  return Simulator::RunFromFiles(paths, options);
}

}  // namespace lyra
