#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {

struct ProjectConfig {
  std::string name;
  std::string top;
  std::vector<std::string> files;
  std::vector<std::string> incdir;
  std::vector<std::string> defines;
  std::vector<std::string> warnings;
  std::string out_dir = "out";
  bool pedantic = false;  // Strict LRM compliance mode

  // Directory where lyra.toml was found
  std::filesystem::path root_dir;
};

// Search for lyra.toml starting from dir, going up to parent dirs.
// Returns nullopt if not found.
auto FindConfig(
    const std::filesystem::path& start_dir = std::filesystem::current_path())
    -> std::optional<std::filesystem::path>;

// Parse lyra.toml file.
// Returns error Diagnostic on parse errors or missing required fields.
auto LoadConfig(const std::filesystem::path& config_path)
    -> lyra::Result<ProjectConfig>;

}  // namespace lyra::driver
