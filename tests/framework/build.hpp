#pragma once

#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace lyra::test {

// Create a unique temp directory using mkdtemp. Returns the absolute path on
// success, or an error description on failure. Never throws.
auto MakeTempCaseDir() -> std::expected<std::filesystem::path, std::string>;

struct BuildAndRunOutcome {
  // Plumbing failure (compile failed, spawn failed, timed out, etc.). When
  // set, the numeric fields below are unspecified.
  std::optional<std::string> error;
  int exit_code = 0;
  std::string stdout_text;
  std::string stderr_text;
};

// Compile <work_dir>/main.cpp + <work_dir>/<top>.hpp against the runtime
// sources collected from `runtime_src_dirs`, with includes rooted at
// `include_root`, producing <work_dir>/program. Then run the program.
//
// Uses RunChildProcess for both compile and run -- no new process helper.
// Returns errors via BuildAndRunOutcome::error; never throws.
auto BuildAndRunEmittedArtifacts(
    const std::filesystem::path& work_dir,
    const std::filesystem::path& include_root,
    const std::vector<std::filesystem::path>& runtime_src_dirs)
    -> BuildAndRunOutcome;

}  // namespace lyra::test
