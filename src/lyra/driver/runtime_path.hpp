#pragma once

#include <filesystem>
#include <string>
#include <vector>

namespace lyra::driver {

// Find liblyra_runtime.so using standard search paths.
// Returns empty path if not found; populates tried_paths with locations
// checked. Search order:
//   1. LYRA_RUNTIME_PATH environment variable
//   2. Bazel runfiles directory
//   3. Sibling to executable
//   4. Current working directory
auto FindRuntimeLibrary(std::vector<std::string>& tried_paths)
    -> std::filesystem::path;

}  // namespace lyra::driver
