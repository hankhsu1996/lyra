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

// Split attached flag forms: -DFOO → -D FOO, -Ipath → -I path, -Wfoo → -W foo
auto PreprocessArgs(std::span<char*> argv) -> std::vector<std::string>;

// Add --top, -I, -D, -W, -f, -F flags to a subcommand.
void AddCompilationFlags(argparse::ArgumentParser& cmd);

// Merge CLI arguments and optional config into a CompilationInput.
// Returns error Diagnostic on failure.
auto BuildInput(
    const argparse::ArgumentParser& cmd,
    const std::optional<ProjectConfig>& config)
    -> lyra::Result<CompilationInput>;

// Load lyra.toml from current directory (or parents).
// Returns nullopt wrapped in Result if no config found.
// Returns error Diagnostic if config found but invalid.
auto LoadOptionalConfig() -> lyra::Result<std::optional<ProjectConfig>>;

}  // namespace lyra::driver
