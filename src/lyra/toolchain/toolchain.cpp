#include "lyra/toolchain/toolchain.hpp"

#include <expected>
#include <format>
#include <regex>
#include <string>

#include "lyra/common/subprocess.hpp"

namespace lyra::toolchain {

namespace {

// Check if command exists using 'which'
auto FindExecutable(const std::string& name) -> std::string {
  auto [status, result] = common::RunSubprocess({"which", name});
  if (status != 0) {
    return "";
  }
  // Remove trailing newline
  if (!result.empty() && result.back() == '\n') {
    result.pop_back();
  }
  return result;
}

}  // namespace

auto CheckCmake(int min_major, int min_minor)
    -> std::expected<ToolInfo, std::string> {
  auto path = FindExecutable("cmake");
  if (path.empty()) {
    return std::unexpected("cmake not found in PATH");
  }

  auto [cmake_status, version_output] =
      common::RunSubprocess({"cmake", "--version"});
  // Parse: "cmake version 3.28.1"
  std::regex version_regex(R"(cmake version (\d+)\.(\d+))");
  std::smatch match;
  if (!std::regex_search(version_output, match, version_regex)) {
    return std::unexpected("could not parse cmake version");
  }

  int major = std::stoi(match[1].str());
  int minor = std::stoi(match[2].str());
  std::string version = std::format("{}.{}", major, minor);

  if (major < min_major || (major == min_major && minor < min_minor)) {
    return std::unexpected(
        std::format(
            "cmake version {} is too old (need >= {}.{})", version, min_major,
            min_minor));
  }

  return ToolInfo{.path = path, .version = version};
}

auto CheckClangPlusPlus() -> std::expected<ToolInfo, std::string> {
  auto path = FindExecutable("clang++");
  if (path.empty()) {
    return std::unexpected(
        "clang++ not found in PATH (required for C++23 support)");
  }

  auto [clang_status, version_output] =
      common::RunSubprocess({"clang++", "--version"});
  // Parse: "clang version 18.0.0" or "Ubuntu clang version 18.0.0"
  std::regex version_regex(R"(clang version (\d+)\.(\d+))");
  std::smatch match;
  std::string version = "unknown";
  if (std::regex_search(version_output, match, version_regex)) {
    version = std::format("{}.{}", match[1].str(), match[2].str());
  }

  return ToolInfo{.path = path, .version = version};
}

auto CheckToolchain() -> ToolchainStatus {
  ToolchainStatus status{.ok = true, .errors = {}};

  if (auto cmake = CheckCmake(); !cmake) {
    status.ok = false;
    status.errors.push_back(cmake.error());
  }

  if (auto clang = CheckClangPlusPlus(); !clang) {
    status.ok = false;
    status.errors.push_back(clang.error());
  }

  return status;
}

}  // namespace lyra::toolchain
