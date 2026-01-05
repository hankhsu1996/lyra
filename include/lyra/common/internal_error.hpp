#pragma once

#include <format>
#include <stdexcept>
#include <string>

namespace lyra::common {

// Exception type for internal Lyra errors (compiler bugs, not user errors)
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

// Helper function to throw internal error (marked [[noreturn]] for
// optimization)
[[noreturn]] inline void ThrowInternalError(
    const char* context, const std::string& detail) {
  throw InternalError(context, detail);
}

}  // namespace lyra::common
