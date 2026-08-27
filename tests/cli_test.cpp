#include <cerrno>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/support/subprocess.hpp"
#include "tests/framework/process.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace {

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

// A directory of this test's own to emit into. The name is drawn rather than
// derived from the test's, because a test that reruns must not inherit what a
// previous run left behind. Returns why it failed rather than throwing, so a
// setup failure is reported as the test failing rather than as a crash.
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

// The smallest design that produces observable output. What these tests are
// about is the project built around a design, so the design itself carries no
// weight beyond proving the program ran.
auto WriteTrivialSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  initial $display(\"ran %0d\", 6 * 7);\n"
      << "endmodule\n";
}

// A design that crosses the DPI-C boundary in both directions (LRM 35): the
// module imports a C function, which calls back the package function the
// package exports.
auto WriteDpiSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "package pkg;\n"
      << "  export \"DPI-C\" function triple;\n"
      << "  function automatic int triple(int x);\n"
      << "    return x * 3;\n"
      << "  endfunction\n"
      << "endpackage\n"
      << "module Test;\n"
      << "  import \"DPI-C\" context function int call_pkg(input int x);\n"
      << "  initial $display(\"dpi %0d\", call_pkg(7));\n"
      << "endmodule\n";
}

// The foreign half, stating no prototype of its own: the generated ABI header
// carries both the import it defines and the export it calls.
auto WriteDpiForeignSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "#include \"dpi.h\"\n"
      << "\n"
      << "int call_pkg(int x) {\n"
      << "  return triple(x);\n"
      << "}\n";
}

TEST(LyraCompile, ProducesPortableBuildableProject) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "compile", "--no-project",   "--top",     "Test",
      "-o",      out_dir.string(), src.string()};
  const auto compile = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(compile.termination, TerminationKind::kExitedNormally)
      << compile.stdout_text << compile.stderr_text;
  ASSERT_EQ(compile.exit_code, 0) << compile.stderr_text;

  const auto program = out_dir / "program";
  ASSERT_TRUE(std::filesystem::exists(program)) << program.string();
  ASSERT_TRUE(std::filesystem::exists(out_dir / "build.sh"));

  // The directory must rebuild standalone, with no Lyra checkout: drop the
  // built program and rebuild via the shipped build.sh from within the dir.
  std::filesystem::remove(program);
  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error();
  const std::vector<std::string> rebuild = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};
  const auto built = RunChildProcess(*sh_or, rebuild, 120s);
  ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
      << built.stdout_text << built.stderr_text;
  ASSERT_EQ(built.exit_code, 0) << built.stderr_text;
  ASSERT_TRUE(std::filesystem::exists(program)) << program.string();

  const auto run = RunChildProcess(program, {}, 30s);
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << run.stdout_text;

  // The recipe also takes a compiler the project was not produced with, which
  // is the rest of what portable means: the headers satisfy a second
  // implementation, and the program still links the runtime library the first
  // one compiled. Skipped where no second implementation is installed.
  auto other_or = lyra::support::FindOnPath("g++");
  if (!other_or) return;
  std::filesystem::remove(program);
  const std::vector<std::string> rebuild_other = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh --cxx '" +
                other_or->string() + "'"};
  const auto other_built = RunChildProcess(*sh_or, rebuild_other, 120s);
  ASSERT_EQ(other_built.exit_code, 0) << other_built.stderr_text;

  const auto other_run = RunChildProcess(program, {}, 30s);
  EXPECT_EQ(other_run.exit_code, 0) << other_run.stderr_text;
  EXPECT_NE(other_run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << other_run.stdout_text;
}

// Re-emitting one directory at the other optimization must rebuild. The recipe
// caches a precompiled header beside the project, and clang refuses one built
// under different options, so the two builds need separate cache entries.
TEST(LyraCompile, RebuildsAfterSwitchingOptimization) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";
  const auto program = out_dir / "program";

  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error();
  const std::vector<std::string> rebuild = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};

  for (const std::string_view mode : {"", "--release"}) {
    std::vector<std::string> args = {"emit", "cpp", "--no-project",  "--top",
                                     "Test", "-o",  out_dir.string()};
    if (!mode.empty()) args.emplace_back(mode);
    args.push_back(src.string());
    const auto emitted = RunChildProcess(lyra, args, 120s);
    ASSERT_EQ(emitted.exit_code, 0) << mode << ": " << emitted.stderr_text;

    const auto built = RunChildProcess(*sh_or, rebuild, 120s);
    ASSERT_EQ(built.exit_code, 0) << mode << ": " << built.stderr_text;

    const auto run = RunChildProcess(program, {}, 30s);
    EXPECT_EQ(run.exit_code, 0) << mode << ": " << run.stderr_text;
    EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
        << mode << " stdout: " << run.stdout_text;
  }
}

TEST(LyraEmit, PortableProjectBuildsItsDpiSources) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteDpiSource(src);
  const auto foreign = *tmp_or / "foreign.c";
  WriteDpiForeignSource(foreign);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "emit",           "cpp",       "--no-project",   "--top",
      "Test",           "-o",        out_dir.string(), "--dpi-link",
      foreign.string(), src.string()};
  const auto emit = RunChildProcess(lyra, args, 60s);
  ASSERT_EQ(emit.termination, TerminationKind::kExitedNormally)
      << emit.stdout_text << emit.stderr_text;
  ASSERT_EQ(emit.exit_code, 0) << emit.stderr_text;

  // The emitted directory carries the whole foreign boundary: the generated
  // prototypes, the standard header they are spelled in, and a copy of the
  // user's source, so it builds where the originals are not reachable.
  EXPECT_TRUE(std::filesystem::exists(out_dir / "dpi.h"));
  EXPECT_TRUE(std::filesystem::exists(out_dir / "svdpi.h"));
  ASSERT_TRUE(std::filesystem::exists(out_dir / "dpi/foreign.c"));
  std::filesystem::remove(foreign);

  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error();
  const std::vector<std::string> build = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};
  const auto built = RunChildProcess(*sh_or, build, 120s);
  ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
      << built.stdout_text << built.stderr_text;
  ASSERT_EQ(built.exit_code, 0) << built.stderr_text;

  const auto run = RunChildProcess(out_dir / "program", {}, 30s);
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("dpi 21"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

TEST(LyraEmit, ReEmitIntoSameDirectorySucceeds) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "emit", "cpp", "--no-project",   "--top",
      "Test", "-o",  out_dir.string(), src.string()};
  // The bundled runtime is copied from a read-only source; emitting twice into
  // the same directory must still succeed (the copy is made writable).
  for (int i = 0; i < 2; ++i) {
    const auto emit = RunChildProcess(lyra, args, 60s);
    ASSERT_EQ(emit.termination, TerminationKind::kExitedNormally)
        << "iteration " << i << ": " << emit.stderr_text;
    ASSERT_EQ(emit.exit_code, 0)
        << "iteration " << i << ": " << emit.stderr_text;
  }
  EXPECT_TRUE(
      std::filesystem::exists(out_dir / "runtime/lib/libcpp_runtime.a"));
}

}  // namespace
