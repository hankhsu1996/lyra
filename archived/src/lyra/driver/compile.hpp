#pragma once

#include <expected>
#include <filesystem>
#include <string>

namespace lyra::driver {

struct ValidatedCompilationInput;

struct CompileOptions {
  std::filesystem::path output_dir;  // Output directory
  std::string name;                  // Executable name
};

// Compile SystemVerilog to a native executable.
// Produces output_dir/<name> with the Lyra runtime linked in.
// Returns the executable path on success, or exit code on failure.
auto Compile(
    const ValidatedCompilationInput& input, const CompileOptions& options)
    -> std::expected<std::filesystem::path, int>;

}  // namespace lyra::driver
