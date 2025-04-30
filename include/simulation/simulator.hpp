#pragma once

#include <string>
#include <vector>

#include "core/simulation_result.hpp"
#include "simulation/simulation_options.hpp"

namespace slang {
class SourceManager;
}

namespace slang::ast {
class Compilation;
}

namespace lyra {

class Simulator {
 public:
  Simulator() = default;

  static auto RunFromSource(
      const std::string& code, const SimulationOptions& options)
      -> SimulationResult;

  static auto RunFromFiles(
      const std::vector<std::string>& paths, const SimulationOptions& options)
      -> SimulationResult;

 private:
  static auto RunWithCompilation(
      std::unique_ptr<slang::ast::Compilation> compilation,
      const SimulationOptions& options) -> SimulationResult;
};

}  // namespace lyra
