#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace lyra::common {

// Execute command with argv array (no shell interpretation).
// argv[0] = program name, argv[1..n] = arguments.
// working_dir: if provided, child process runs in this directory.
// Returns (exit_code, merged_stdout_stderr).
// If exec fails, returns (-1, error_message).
auto RunSubprocess(
    const std::vector<std::string>& argv,
    const std::optional<std::filesystem::path>& working_dir = std::nullopt)
    -> std::pair<int, std::string>;

}  // namespace lyra::common
