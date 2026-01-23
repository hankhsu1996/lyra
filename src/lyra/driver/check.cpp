#include "check.hpp"

#include "frontend.hpp"

namespace lyra::driver {

auto Check(const CompilationInput& input) -> int {
  auto parse_result = LoadFiles(input);
  if (!parse_result) {
    return 1;
  }
  return 0;
}

}  // namespace lyra::driver
