#pragma once

#include <filesystem>
#include <span>
#include <string>
#include <vector>

namespace lyra::test {

// Result of compiling DPI companion C sources for testing.
struct DpiCompileResult {
  std::vector<std::filesystem::path> link_inputs;
  std::string error;
  [[nodiscard]] auto Ok() const -> bool {
    return error.empty();
  }
};

// Compile DPI companion C source files into shared libraries in work_dir.
// Uses DetectToolchain() for the C compiler and RunSubprocess() for
// execution with captured stderr. Returns produced shared-library paths in
// stable order, or an error with actionable diagnostics.
auto CompileDpiSources(
    std::span<const std::string> dpi_sources,
    const std::filesystem::path& work_dir) -> DpiCompileResult;

}  // namespace lyra::test
