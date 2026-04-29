#include <chrono>
#include <filesystem>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <vector>

#include "tests/framework/build.hpp"
#include "tests/framework/process.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::BuildAndRunEmittedArtifacts;
using lyra::test::MakeTempCaseDir;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;

namespace {

struct RunOutcome {
  int exit_code = 0;
  std::string stdout_text;
  std::string stderr_text;
};

void RunSvCase(const std::string& sv_runfiles_path, RunOutcome& out) {
  std::string err;
  std::unique_ptr<Runfiles> rf{Runfiles::CreateForTest(&err)};
  ASSERT_TRUE(rf) << err;

  const std::filesystem::path lyra_exe = rf->Rlocation("_main/lyra");
  ASSERT_FALSE(lyra_exe.empty());
  ASSERT_TRUE(std::filesystem::exists(lyra_exe)) << lyra_exe;

  const std::filesystem::path sv_path = rf->Rlocation(sv_runfiles_path);
  ASSERT_FALSE(sv_path.empty());
  ASSERT_TRUE(std::filesystem::exists(sv_path)) << sv_path;

  const std::filesystem::path engine_hpp =
      rf->Rlocation("_main/include/lyra/runtime/engine.hpp");
  ASSERT_FALSE(engine_hpp.empty());
  ASSERT_TRUE(std::filesystem::exists(engine_hpp)) << engine_hpp;
  const std::filesystem::path include_root =
      engine_hpp.parent_path().parent_path().parent_path();

  const std::filesystem::path engine_cpp =
      rf->Rlocation("_main/src/lyra/runtime/engine.cpp");
  ASSERT_FALSE(engine_cpp.empty());
  ASSERT_TRUE(std::filesystem::exists(engine_cpp)) << engine_cpp;

  const std::filesystem::path base_cpp =
      rf->Rlocation("_main/src/lyra/base/internal_error.cpp");
  ASSERT_FALSE(base_cpp.empty());
  ASSERT_TRUE(std::filesystem::exists(base_cpp)) << base_cpp;

  const std::vector<std::filesystem::path> runtime_src_dirs = {
      engine_cpp.parent_path(), base_cpp.parent_path()};

  auto work_or = MakeTempCaseDir();
  ASSERT_TRUE(work_or) << work_or.error();
  const auto& work = *work_or;

  const std::vector<std::string> emit_argv = {
      "emit", "cpp",       "--no-project", "--top",
      "Top",  "--out-dir", work.string(),  sv_path.string(),
  };
  auto emit = RunChildProcess(lyra_exe, emit_argv, std::chrono::seconds{30});
  ASSERT_EQ(emit.termination, TerminationKind::kExitedNormally)
      << "emit terminated abnormally:\nstdout:\n"
      << emit.stdout_text << "\nstderr:\n"
      << emit.stderr_text;
  ASSERT_EQ(emit.exit_code, 0) << "emit failed:\nstdout:\n"
                               << emit.stdout_text << "\nstderr:\n"
                               << emit.stderr_text;

  auto outcome =
      BuildAndRunEmittedArtifacts(work, include_root, runtime_src_dirs);
  ASSERT_FALSE(outcome.error.has_value()) << *outcome.error;
  out.exit_code = outcome.exit_code;
  out.stdout_text = std::move(outcome.stdout_text);
  out.stderr_text = std::move(outcome.stderr_text);
}

void RunOneCase(const std::string& sv_runfiles_path) {
  RunOutcome outcome;
  RunSvCase(sv_runfiles_path, outcome);
  EXPECT_EQ(outcome.exit_code, 0)
      << "stdout=" << outcome.stdout_text << " stderr=" << outcome.stderr_text;
}

void RunOneCaseExpectingStdout(
    const std::string& sv_runfiles_path, const std::string& expected_stdout) {
  RunOutcome outcome;
  RunSvCase(sv_runfiles_path, outcome);
  ASSERT_EQ(outcome.exit_code, 0) << outcome.stderr_text;
  EXPECT_EQ(outcome.stdout_text, expected_stdout);
}

TEST(CliRun, BlockingAssign) {
  RunOneCase("_main/tests/cases/run/blocking_assign.sv");
}

TEST(CliRun, LocalVarAssign) {
  RunOneCase("_main/tests/cases/run/local_var_assign.sv");
}

TEST(CliRun, MemberAdd) {
  RunOneCase("_main/tests/cases/run/member_add.sv");
}

TEST(CliRun, NestedBlock) {
  RunOneCase("_main/tests/cases/run/nested_block.sv");
}

TEST(CliRun, DelayDefaultTimescale) {
  RunOneCaseExpectingStdout(
      "_main/tests/cases/run/delay_default_timescale.sv", "a\nb\n");
}

TEST(CliRun, DelayExplicitTimescale) {
  RunOneCaseExpectingStdout(
      "_main/tests/cases/run/delay_explicit_timescale.sv", "a\nb\nc\n");
}

TEST(CliRun, DelayOrdering) {
  RunOneCaseExpectingStdout(
      "_main/tests/cases/run/delay_ordering.sv", "a\nc\nb\n");
}

TEST(CliRun, DelayZeroInactive) {
  RunOneCaseExpectingStdout(
      "_main/tests/cases/run/delay_zero_inactive.sv", "a\nc\nb\n");
}

}  // namespace
