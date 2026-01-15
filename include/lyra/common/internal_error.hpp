#pragma once

#include <format>
#include <stdexcept>
#include <string>

namespace lyra::common {

// Exception for compiler bugs - situations that should never happen.
// Use when an invariant is violated or code reaches an "impossible" state.
class InternalError : public std::runtime_error {
 public:
  InternalError(const char* context, const std::string& detail)
      : std::runtime_error(
            std::format(
                "Internal error in {}: {}\n"
                "This is a bug in Lyra. Please report at: "
                "https://github.com/hankhsu1996/lyra/issues",
                context, detail)) {
  }
};

}  // namespace lyra::common
