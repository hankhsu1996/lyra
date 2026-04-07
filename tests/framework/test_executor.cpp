#include "tests/framework/test_executor.hpp"

#include <exception>
#include <filesystem>
#include <format>
#include <optional>
#include <system_error>

#include "tests/framework/aot_backend.hpp"
#include "tests/framework/jit_backend.hpp"
#include "tests/framework/lli_backend.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {
namespace {

auto NeedsWorkDirectory(const TestCase& tc) -> bool {
  return tc.IsMultiFile() || !tc.expected_files.empty() ||
         !tc.dpi_sources.empty();
}

}  // namespace

auto ExecuteTestCase(
    const TestCase& test_case, BackendKind backend, bool force_two_state)
    -> CaseExecutionResult {
  try {
    std::optional<ScopedTempDirectory> workdir_guard;
    std::filesystem::path work_directory;

    if (NeedsWorkDirectory(test_case)) {
      work_directory = MakeUniqueTempPath(test_case.name);

      std::error_code ec;
      std::filesystem::create_directories(work_directory, ec);
      if (ec) {
        CaseExecutionResult result;
        result.execution.outcome = ExecutionOutcome::kInfraError;
        result.execution.error_message = std::format(
            "failed to create work directory '{}': {}", work_directory.string(),
            ec.message());
        return result;
      }

      workdir_guard.emplace(work_directory);
    }

    switch (backend) {
      case BackendKind::kJit:
        return RunJitBackend(test_case, work_directory, force_two_state);
      case BackendKind::kAot:
        return RunAotBackend(test_case, work_directory);
      case BackendKind::kLli:
        return RunLliBackend(test_case, work_directory);
    }

    CaseExecutionResult result;
    result.execution.outcome = ExecutionOutcome::kInfraError;
    result.execution.error_message = "unknown backend";
    return result;
  } catch (const std::exception& e) {
    CaseExecutionResult result;
    result.execution.outcome = ExecutionOutcome::kInfraError;
    result.execution.error_message =
        std::format("ExecuteTestCase infrastructure failure: {}", e.what());
    return result;
  }
}

}  // namespace lyra::test
