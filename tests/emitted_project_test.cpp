// What an emitted C++ project is worth once it has left the compiler: that the
// directory rebuilds standalone through the recipe it ships, that a second
// toolchain accepts its headers and still links the runtime the first one
// compiled, that it is built as wide as a caller asked, and that a build whose
// prepared header is refused still produces a program.
//
// Every case here builds a project with the host compiler, several of them more
// than once, and no two can share what one prepared -- a prepared header is
// bound to the paths of the headers it was made from, and every project carries
// its own copy. So the file costs what it asserts, and it is answered on the
// schedule that answers everything else about that path. Cases about the
// command line itself belong beside it in `cli_test.cpp`, which gates.

#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/support/subprocess.hpp"
#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/process.hpp"

namespace {

using lyra::test::MakeScratchDir;
using lyra::test::ResolveLyra;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using lyra::test::WriteTrivialSource;
using namespace std::chrono_literals;

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

TEST(LyraEmittedProject, ProducesPortableBuildableProject) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "compile", "--top", "Test", "-o", out_dir.string(), src.string()};
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
TEST(LyraEmittedProject, RebuildsAfterSwitchingOptimization) {
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
    std::vector<std::string> args = {"emit", "cpp", "--top",
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

// Nothing compiled in advance decides whether a build succeeds. One build
// compiles a header and caches it; the next is handed it back after every
// header's timestamp has moved under it, which is what a checkout leaves and
// what the compiler refuses to accept -- and that build still owes a program.
// The timestamps are moved here by hand because Lyra no longer moves them
// itself and a checkout is not something a test can stage.
TEST(LyraEmittedProject, BuildsEvenWhenThePrecompiledHeaderIsRefused) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  // A cache of this test's own, so what the second build is handed is what the
  // first one left rather than whatever the developer's cache happens to hold.
  const std::vector<std::string> args = {
      "compile",
      "--top",
      "Test",
      "-o",
      out_dir.string(),
      "--pch-cache-dir",
      (*tmp_or / "pch").string(),
      src.string()};
  const auto first = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(first.termination, TerminationKind::kExitedNormally)
      << first.stdout_text << first.stderr_text;
  ASSERT_EQ(first.exit_code, 0) << first.stderr_text;

  const auto headers = out_dir / "runtime" / "include";
  ASSERT_TRUE(std::filesystem::exists(headers)) << headers.string();
  std::size_t moved = 0;
  for (const auto& entry :
       std::filesystem::recursive_directory_iterator(headers)) {
    if (!entry.is_regular_file()) continue;
    std::filesystem::last_write_time(
        entry.path(), std::filesystem::last_write_time(entry.path()) + 24h);
    ++moved;
  }
  ASSERT_GT(moved, 0U) << "no header to move under " << headers.string();

  const auto second = RunChildProcess(lyra, args, 120s);
  EXPECT_EQ(second.exit_code, 0)
      << "a build was failed by the header it had prepared for itself:\n"
      << second.stderr_text;

  const auto run = RunChildProcess(out_dir / "program", {}, 30s);
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

// How many units are compiled at once is the caller's to say, at both things
// that build a design, and it changes nothing about the program produced. What
// a width buys is measured elsewhere; asserting on it here would be asserting
// on the machine the test happens to run on.
TEST(LyraEmittedProject, TakesTheWidthItIsGiven) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";
  const auto program = out_dir / "program";

  const std::vector<std::string> args = {
      "compile", "--top", "Test",           "-j",
      "4",       "-o",    out_dir.string(), src.string()};
  const auto compiled = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(compiled.exit_code, 0) << compiled.stderr_text;
  const auto ran = RunChildProcess(program, {}, 30s);
  EXPECT_NE(ran.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << ran.stdout_text;

  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error();
  // Zero asks the recipe for one compile per processor, which is the spelling
  // a caller uses to say the machine is its own.
  for (const std::string_view width : {"1", "0"}) {
    std::filesystem::remove(program);
    const std::vector<std::string> rebuild = {
        "-c",
        std::format("cd '{}' && sh build.sh -j {}", out_dir.string(), width)};
    const auto built = RunChildProcess(*sh_or, rebuild, 120s);
    ASSERT_EQ(built.exit_code, 0) << width << ": " << built.stderr_text;

    const auto run = RunChildProcess(program, {}, 30s);
    EXPECT_EQ(run.exit_code, 0) << width << ": " << run.stderr_text;
    EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
        << width << " stdout: " << run.stdout_text;
  }
}

// Building twice into one directory, with nothing about the design changed.
// The second run rewrites the same runtime headers, and the precompiled header
// it was handed is validated by their modification times rather than by the
// content its cache key is built from -- so a generator that rewrote an
// unchanged file would make the build reject a header it had just produced.
TEST(LyraEmittedProject, BuildsTwiceIntoOneDirectory) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";
  const auto program = out_dir / "program";

  const std::vector<std::string> args = {
      "compile", "--top", "Test", "-o", out_dir.string(), src.string()};
  for (const std::string_view pass : {"first", "second"}) {
    const auto compiled = RunChildProcess(lyra, args, 120s);
    ASSERT_EQ(compiled.exit_code, 0) << pass << ": " << compiled.stderr_text;

    const auto run = RunChildProcess(program, {}, 30s);
    EXPECT_EQ(run.exit_code, 0) << pass << ": " << run.stderr_text;
    EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
        << pass << " stdout: " << run.stdout_text;
  }
}

TEST(LyraEmittedProject, PortableProjectBuildsItsDpiSources) {
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
      "emit",           "cpp",        "--top",          "Test",      "-o",
      out_dir.string(), "--dpi-link", foreign.string(), src.string()};
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

}  // namespace
