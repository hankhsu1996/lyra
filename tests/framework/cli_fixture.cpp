#include "tests/framework/cli_fixture.hpp"

#include <cerrno>
#include <chrono>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <nlohmann/json.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/driver/subprocess.hpp"
#include "tests/framework/process.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::test {

namespace {

using bazel::tools::cpp::runfiles::Runfiles;

// Where this run was given something the test declared it needs, named from the
// workspace root.
auto ResolveDeclaredInput(std::string_view workspace_path)
    -> std::filesystem::path {
  std::string err;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&err)};
  EXPECT_TRUE(runfiles) << err;
  if (!runfiles) return {};
  return runfiles->Rlocation(std::format("_main/{}", workspace_path));
}

}  // namespace

auto MakeScratchDir() -> std::expected<std::filesystem::path, std::string> {
  const auto base = std::filesystem::temp_directory_path() / "lyra-XXXXXX";
  std::string templ = base.string();
  if (mkdtemp(templ.data()) == nullptr) {
    return std::unexpected(
        std::format(
            "mkdtemp('{}') failed: {}", base.string(), std::strerror(errno)));
  }
  return std::filesystem::path(templ);
}

auto ResolveLyra() -> std::filesystem::path {
  return ResolveDeclaredInput("lyra");
}

auto ResolveShippedExamples() -> std::filesystem::path {
  return ResolveDeclaredInput("examples");
}

auto RunLyraFrom(
    const std::filesystem::path& lyra, const std::filesystem::path& dir,
    std::string_view args, std::chrono::seconds timeout) -> ProcessOutcome {
  auto sh_or = lyra::driver::FindOnPath("sh");
  EXPECT_TRUE(sh_or.has_value());
  if (!sh_or) return {};
  const std::vector<std::string> argv = {
      "-c",
      std::format("cd '{}' && '{}' {}", dir.string(), lyra.string(), args)};
  return RunChildProcess(*sh_or, argv, timeout);
}

auto FindDefaultCxx() -> std::optional<std::filesystem::path> {
  auto cxx_or = lyra::driver::FindOnPath("clang++");
  if (!cxx_or) return std::nullopt;
  if (cxx_or->filename().string().find("clang") == std::string::npos) {
    return std::nullopt;
  }
  return *cxx_or;
}

auto WriteTrivialSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  initial $display(\"ran %0d\", 6 * 7);\n"
      << "endmodule\n";
}

auto ReadJson(const std::filesystem::path& path) -> nlohmann::json {
  std::ifstream in(path);
  return nlohmann::json::parse(in, nullptr, false);
}

}  // namespace lyra::test
