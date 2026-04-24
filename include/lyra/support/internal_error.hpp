#pragma once

#include <stdexcept>
#include <string>
#include <string_view>

namespace lyra::support {

// Exception for compiler bugs: invariant violations, "impossible" states,
// and unimplemented paths that the rest of the pipeline must not reach.
class InternalError : public std::runtime_error {
 public:
  explicit InternalError(std::string_view message)
      : std::runtime_error(std::string{message}) {
  }
};

}  // namespace lyra::support
