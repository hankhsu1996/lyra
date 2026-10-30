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

auto WriteForeignCodeDesign(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  import \"DPI-C\" context function int from_object(int x);\n"
      << "  import \"DPI-C\" context function int from_archive(int x);\n"
      << "  import \"DPI-C\" context function int from_shared(int x);\n"
      << "  import \"DPI-C\" context function int from_plain(int x);\n"
      << "  export \"DPI-C\" function twice;\n"
      << "  function int twice(int x);\n"
      << "    return 2 * x;\n"
      << "  endfunction\n"
      << "  initial $display(\"foreign %0d %0d %0d %0d\", from_object(10),\n"
      << "                   from_archive(10), from_shared(10), "
         "from_plain(10));\n"
      << "endmodule\n";
}

auto CompileForeignCodeInto(const std::filesystem::path& dir)
    -> ProcessOutcome {
  return RunShellScript(
      R"sh(set -e
cd "$0"
mkdir -p shared plain
define() {
  printf 'int twice(int x);\nint %s(int x) { return twice(x) + %s; }\n' \
    "$1" "$2" > "$3"
}
define from_object 1 object.c
define from_archive 2 archive.c
define from_shared 3 shared.c
define from_plain 4 plain.c
define from_plain 40 decoy.c
cc -c -fPIC object.c -o object.o
cc -c -fPIC archive.c -o archive.o
ar rcs libarchive.a archive.o
cc -shared -fPIC -Wl,-soname,libshared.so.1 shared.c -o shared/libshared.so.1
ln -sf libshared.so.1 shared/libshared.so
cc -shared -fPIC plain.c -o plain/libplain.so
cc -shared -fPIC decoy.c -o shared/libplain.so
printf '#! SV_LIBRARIES\n  # the one declaring no soname\n\n  libplain\n' \
  > plain/libraries
)sh",
      std::vector<std::string>{dir.string()});
}

auto ReadJson(const std::filesystem::path& path) -> nlohmann::json {
  std::ifstream in(path);
  return nlohmann::json::parse(in, nullptr, false);
}

}  // namespace lyra::test
