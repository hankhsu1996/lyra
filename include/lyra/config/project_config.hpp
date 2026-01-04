#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace lyra::config {

struct ProjectConfig {
  std::string name;
  std::string top;
  std::vector<std::string> files;
  std::vector<std::string> incdir;
  std::string out_dir = "out";

  // Directory where lyra.toml was found
  std::filesystem::path root_dir;
};

// Search for lyra.toml starting from dir, going up to parent dirs
// Returns nullopt if not found
auto FindConfig(
    const std::filesystem::path& start_dir = std::filesystem::current_path())
    -> std::optional<std::filesystem::path>;

// Parse lyra.toml file
// Throws on parse errors or missing required fields
auto LoadConfig(const std::filesystem::path& config_path) -> ProjectConfig;

}  // namespace lyra::config
