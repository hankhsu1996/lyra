#pragma once

#include <argparse/argparse.hpp>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include "config.hpp"
#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {

enum class Backend { kJit, kLli, kMir };
enum class DumpFormat { kHir, kMir, kLlvm };

auto ParseBackend(const std::string& s) -> lyra::Result<Backend>;
auto ParseDumpFormat(const std::string& s) -> lyra::Result<DumpFormat>;

// Split attached flag forms: -DFOO -> -D FOO, -Ipath -> -I path, -Wfoo -> -W
// foo
auto PreprocessArgs(std::span<char*> argv) -> std::vector<std::string>;

// Add --top, -I, -D, -W, -f, -F flags to a subcommand.
void AddCompilationFlags(argparse::ArgumentParser& cmd);

// Merge CLI arguments and optional config into a CompilationInput.
// Returns error Diagnostic on failure.
auto BuildInput(
    const argparse::ArgumentParser& cmd,
    const std::optional<ProjectConfig>& config)
    -> lyra::Result<CompilationInput>;

// Load lyra.toml, requiring it to exist (project mode).
// Returns error Diagnostic if not found or invalid.
auto LoadProjectConfig() -> lyra::Result<ProjectConfig>;

// Prepare a fully-populated CompilationInput for a command.
// Handles both project mode (require lyra.toml) and ad-hoc mode (no_project).
// - In project mode: fs_base_dir = project root (lyra.toml directory)
// - In ad-hoc mode: fs_base_dir = effective CWD
auto PrepareInput(const argparse::ArgumentParser& cmd, bool no_project)
    -> lyra::Result<CompilationInput>;

}  // namespace lyra::driver
