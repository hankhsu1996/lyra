#include "tests/framework/cli_fixture.hpp"

#include <cerrno>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <optional>
#include <string>

#include "lyra/support/subprocess.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::test {

namespace {

using bazel::tools::cpp::runfiles::Runfiles;

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
  std::string err;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&err)};
  EXPECT_TRUE(runfiles) << err;
  return runfiles ? std::filesystem::path(runfiles->Rlocation("_main/lyra"))
                  : std::filesystem::path{};
}

auto FindDefaultCxx() -> std::optional<std::filesystem::path> {
  auto cxx_or = lyra::support::FindOnPath("clang++");
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

}  // namespace lyra::test
