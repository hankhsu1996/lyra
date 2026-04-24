#pragma once

#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::driver {

// Find a runtime library by name using standard search paths.
// Returns empty path if not found; populates tried_paths with locations
// checked. Search order:
//   1. LYRA_RUNTIME_PATH environment variable
//   2. Bazel runfiles directory
//   3. Sibling to executable
//   4. Current working directory
auto FindRuntimeLibrary(
    std::string_view lib_name, std::vector<std::string>& tried_paths)
    -> std::filesystem::path;

// Find a runtime support file (header) by its repo-relative path.
// Search order:
//   1. Bazel runfiles directory
//   2. Relative to current working directory
auto FindRuntimeSupportFile(
    std::string_view repo_relative_path, std::vector<std::string>& tried_paths)
    -> std::filesystem::path;

}  // namespace lyra::driver
