#pragma once

#include <expected>
#include <string>
#include <vector>

namespace lyra::toolchain {

struct ToolInfo {
  std::string path;
  std::string version;
};

struct ToolchainStatus {
  bool ok;
  std::vector<std::string> errors;
};

// Check if cmake is available and meets minimum version (default 3.20)
auto CheckCmake(int min_major = 3, int min_minor = 20)
    -> std::expected<ToolInfo, std::string>;

// Check if clang++ is available
auto CheckClangPlusPlus() -> std::expected<ToolInfo, std::string>;

// Run all toolchain checks, return aggregated status
auto CheckToolchain() -> ToolchainStatus;

}  // namespace lyra::toolchain
