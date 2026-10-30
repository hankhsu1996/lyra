#include "tests/framework/cli_fixture.hpp"

#include <cstdlib>
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

#include "conformance/framework/process.hpp"
#include "lyra/driver/subprocess.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::test {

using conformance::framework::ProcessOutcome;
using conformance::framework::RunShellScript;

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

auto ResolveLyra() -> std::filesystem::path {
  return ResolveDeclaredInput("lyra");
}

auto ResolveShippedExamples() -> std::filesystem::path {
  return ResolveDeclaredInput("examples");
}

auto ResolveLlvmNm() -> std::filesystem::path {
  std::string err;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&err)};
  EXPECT_TRUE(runfiles) << err;
  const char* location = std::getenv("LLVM_NM");
  EXPECT_NE(location, nullptr) << "the test declares no LLVM_NM";
  if (!runfiles || location == nullptr) return {};
  return runfiles->Rlocation(location);
}

auto RunLyraFrom(
    const std::filesystem::path& lyra, const std::filesystem::path& dir,
    std::string_view args) -> ProcessOutcome {
  return RunShellScript(
      std::format("cd '{}' && '{}' {}", dir.string(), lyra.string(), args));
}

auto FindClang() -> std::optional<std::filesystem::path> {
  auto cxx_or = lyra::driver::FindOnPath("clang++");
  if (!cxx_or) return std::nullopt;
  return *std::move(cxx_or);
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
