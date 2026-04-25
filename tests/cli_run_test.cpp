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

TEST(CliRun, BlockingAssign) {
  std::string err;
  std::unique_ptr<Runfiles> rf{Runfiles::CreateForTest(&err)};
  ASSERT_TRUE(rf) << err;

  const std::filesystem::path lyra_exe = rf->Rlocation("_main/lyra");
  ASSERT_FALSE(lyra_exe.empty());
  ASSERT_TRUE(std::filesystem::exists(lyra_exe)) << lyra_exe;

  const std::filesystem::path sv_path =
      rf->Rlocation("_main/tests/cases/run/blocking_assign.sv");
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
  const std::filesystem::path runtime_src_dir = engine_cpp.parent_path();

  auto work_or = MakeTempCaseDir();
  ASSERT_TRUE(work_or) << work_or.error();
  const auto& work = *work_or;

  // Step 1: emit. argv is argv[1..]; lyra_exe becomes argv[0] inside
  // RunChildProcess.
  const std::vector<std::string> emit_argv = {
      "emit", "cpp",       "--no-project", "--top",
      "Top",  "--out-dir", work.string(),  sv_path.string(),
  };
  auto emit = RunChildProcess(lyra_exe, emit_argv, std::chrono::seconds{30});
  ASSERT_EQ(emit.termination, TerminationKind::kExitedNormally)
      << "emit failed:\nstdout:\n"
      << emit.stdout_text << "\nstderr:\n"
      << emit.stderr_text;

  // Step 2: build + run.
  auto outcome =
      BuildAndRunEmittedArtifacts(work, include_root, runtime_src_dir);
  ASSERT_FALSE(outcome.error.has_value()) << *outcome.error;
  EXPECT_EQ(outcome.exit_code, 0)
      << "stdout=" << outcome.stdout_text << " stderr=" << outcome.stderr_text;
}

}  // namespace
