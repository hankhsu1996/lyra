#include "lyra/toolchain/toolchain.hpp"

#include <array>
#include <cstdio>
#include <expected>
#include <format>
#include <memory>
#include <regex>
#include <string>

namespace lyra::toolchain {

namespace {

// Execute command and capture stdout
auto ExecuteCommand(const std::string& cmd) -> std::string {
  std::array<char, 256> buffer{};
  std::string result;
  // NOLINTBEGIN(misc-include-cleaner): popen/pclose are in <cstdio>
  std::unique_ptr<FILE, decltype(&pclose)> pipe(
      popen(cmd.c_str(), "r"), pclose);
  // NOLINTEND(misc-include-cleaner)
  if (!pipe) {
    return "";
  }
  while (fgets(buffer.data(), static_cast<int>(buffer.size()), pipe.get()) !=
         nullptr) {
    result += buffer.data();
  }
  return result;
}

// Check if command exists using 'which'
auto FindExecutable(const std::string& name) -> std::string {
  std::string result = ExecuteCommand("which " + name + " 2>/dev/null");
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

  auto version_output = ExecuteCommand("cmake --version 2>/dev/null");
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

  auto version_output = ExecuteCommand("clang++ --version 2>/dev/null");
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
