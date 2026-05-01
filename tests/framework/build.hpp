#pragma once

#include <expected>
#include <filesystem>
#include <optional>
#include <span>
#include <string>

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

// Compile <work_dir>/main.cpp against the prebuilt C++ runtime
// `cpp_runtime`, with includes rooted at `include_root` plus each path in
// `extra_include_roots`, producing <work_dir>/program. Then run the program.
//
// Uses RunChildProcess for both compile and run -- no new process helper.
// Returns errors via BuildAndRunOutcome::error; never throws.
auto BuildAndRunEmittedArtifacts(
    const std::filesystem::path& work_dir,
    const std::filesystem::path& include_root,
    const std::filesystem::path& cpp_runtime,
    std::span<const std::filesystem::path> extra_include_roots)
    -> BuildAndRunOutcome;

}  // namespace lyra::test
