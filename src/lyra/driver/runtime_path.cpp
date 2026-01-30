#include "runtime_path.hpp"

#include <cstdlib>
#include <filesystem>
#include <format>
#include <string>
#include <vector>

namespace lyra::driver {

auto FindRuntimeLibrary(std::vector<std::string>& tried_paths)
    -> std::filesystem::path {
  namespace fs = std::filesystem;
  constexpr auto kLibName = "liblyra_runtime.so";

  // 1. LYRA_RUNTIME_PATH environment variable
  if (const char* env_path = std::getenv("LYRA_RUNTIME_PATH")) {
    if (fs::exists(env_path)) {
      return env_path;
    }
    tried_paths.emplace_back(std::format("LYRA_RUNTIME_PATH={}", env_path));
  }

  // 2. Relative to executable (Bazel runfiles, sibling)
  fs::path exe_path;
  try {
    exe_path = fs::read_symlink("/proc/self/exe");
  } catch (const fs::filesystem_error& e) {
    tried_paths.emplace_back(
        std::format(
            "/proc/self/exe ({}; non-Linux or sandboxed?)",
            e.code().message()));
  }

  if (!exe_path.empty()) {
    // Bazel runfiles path
    auto runfiles_path =
        fs::path(exe_path.string() + ".runfiles") / "_main" / kLibName;
    tried_paths.push_back(runfiles_path.string());
    if (fs::exists(runfiles_path)) {
      return runfiles_path;
    }

    // Sibling to executable
    auto sibling_path = exe_path.parent_path() / kLibName;
    tried_paths.push_back(sibling_path.string());
    if (fs::exists(sibling_path)) {
      return sibling_path;
    }
  }

  // 3. Current working directory
  auto cwd_path = fs::current_path() / kLibName;
  tried_paths.push_back(cwd_path.string());
  if (fs::exists(cwd_path)) {
    return cwd_path;
  }

  return {};
}

}  // namespace lyra::driver
