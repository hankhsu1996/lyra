#pragma once

#include <string>
#include <vector>

#include "lyra/driver/driver_options.hpp"
#include "lyra/driver/driver_result.hpp"

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
      -> DriverResult;

  static auto RunFromFiles(
      const std::vector<std::string>& paths, const DriverOptions& options = {})
      -> DriverResult;

 private:
  static auto RunWithCompilation(
      std::unique_ptr<slang::ast::Compilation> compilation,
      const DriverOptions& options) -> DriverResult;
};

}  // namespace lyra::driver
