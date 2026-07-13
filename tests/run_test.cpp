#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <vector>

#include "lyra/support/subprocess.hpp"
#include "tests/framework/build.hpp"
#include "tests/framework/process.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace {

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::MakeTempCaseDir;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

auto ResolveLyra() -> std::filesystem::path {
  std::string err;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&err)};
  EXPECT_TRUE(runfiles) << err;
  return runfiles ? std::filesystem::path(runfiles->Rlocation("_main/lyra"))
                  : std::filesystem::path{};
}

auto WriteTrivialSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  initial $display(\"ran %0d\", 6 * 7);\n"
      << "endmodule\n";
}

// A two-level hierarchy: the top instantiates a submodule, and each level runs
// an initial block. Elaborating it exercises the design-root unit constructing
// the top and the top constructing its submodule as owned children.
auto WriteHierarchicalSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Leaf;\n"
      << "  initial $display(\"leaf ran\");\n"
      << "endmodule\n"
      << "module Test;\n"
      << "  Leaf a();\n"
      << "  initial $display(\"top ran\");\n"
      << "endmodule\n";
}

// Straight-line procedural code over the integral and string value domains:
// module and process variables, arithmetic, comparison, a conditional
// expression, a loop with early exits, a subroutine that writes its own
// parameter, and a `$display` that formats values rather than a bare literal.
auto WriteProceduralSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  int total = 0;\n"
      << "  string name = \"lyra\";\n"
      << "  bit [3:0] narrow;\n"
      << "  function automatic int scale(int n);\n"
      << "    n = n * 10;\n"
      << "    return n;\n"
      << "  endfunction\n"
      << "  initial begin\n"
      << "    for (int k = 0; k < 5; k++) begin\n"
      << "      if (k == 2) continue;\n"
      << "      if (k == 4) break;\n"
      << "      total = total + k;\n"
      << "    end\n"
      << "    narrow = 8'hFF;\n"
      << "    $display(\"total=%0d\", total);\n"
      << "    $display(\"pick=%0d\", total > 3 ? 111 : 222);\n"
      << "    $display(\"scaled=%0d\", scale(4));\n"
      << "    $display(\"narrow=%0d\", narrow);\n"
      << "    $display(\"name=%s eq=%0d\", name, name == \"lyra\");\n"
      << "    if (total > 0 && total < 100) $display(\"in range\");\n"
      << "  end\n"
      << "endmodule\n";
}

TEST(LyraRun, ExecutesSourceEndToEnd) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);

  const std::vector<std::string> args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(run.termination, TerminationKind::kExitedNormally)
      << run.stdout_text << run.stderr_text;
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

// The JIT backend elaborates the design through the synthesized design-root
// unit's construct: it builds the top as an owned child, and the top builds its
// submodule, so both levels' initial blocks run. This is the execution-backend
// counterpart of the C++ backend's constructor-driven elaboration.
TEST(LyraRun, JitElaboratesHierarchyThroughDesignRoot) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteHierarchicalSource(src);

  const std::vector<std::string> args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(run.termination, TerminationKind::kExitedNormally)
      << run.stdout_text << run.stderr_text;
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("top ran"), std::string::npos)
      << "stdout: " << run.stdout_text;
  EXPECT_NE(run.stdout_text.find("leaf ran"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

// The execution backend runs procedural code: a variable is a runtime-owned
// storage cell reached through a member place, an expression is a library call
// over the value's domain, and structured control flow is a CFG. The two
// backends must agree, so the same source is run through both and the outputs
// compared rather than matched against a transcript written here.
TEST(LyraRun, JitAndCppAgreeOnProceduralCode) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteProceduralSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_NE(jit.stdout_text.find("name=lyra eq=1"), std::string::npos)
      << "stdout: " << jit.stdout_text;
  EXPECT_NE(jit.stdout_text.find("scaled=40"), std::string::npos)
      << "stdout: " << jit.stdout_text;
}

TEST(LyraCompile, ProducesPortableBuildableProject) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
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
}

TEST(LyraEmit, ReEmitIntoSameDirectorySucceeds) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
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
