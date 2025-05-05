#pragma once

#include <string>
#include <vector>

#include "core/simulation_result.hpp"
#include "driver/driver_options.hpp"

namespace slang {
class SourceManager;
}

namespace slang::ast {
class Compilation;
}

namespace lyra::driver {

class Driver {
 public:
  Driver() = default;

  static auto RunFromSource(
      const std::string& code, const DriverOptions& options = {})
      -> SimulationResult;

  static auto RunFromFiles(
      const std::vector<std::string>& paths, const DriverOptions& options = {})
      -> SimulationResult;

 private:
  static auto RunWithCompilation(
      std::unique_ptr<slang::ast::Compilation> compilation,
      const DriverOptions& options) -> SimulationResult;
};

}  // namespace lyra::driver
