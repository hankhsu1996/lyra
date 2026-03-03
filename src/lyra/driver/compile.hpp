#pragma once

#include <expected>
#include <filesystem>
#include <string>

namespace lyra::driver {

struct CompilationInput;

struct CompileOptions {
  std::filesystem::path output_dir;  // Bundle output directory
  std::string name;                  // Executable name
};

// Compile SystemVerilog to a native executable bundle.
// Produces output_dir/bin/<name> + output_dir/lib/liblyra_runtime.so
// Returns the executable path on success, or exit code on failure.
auto Compile(const CompilationInput& input, const CompileOptions& options)
    -> std::expected<std::filesystem::path, int>;

}  // namespace lyra::driver
