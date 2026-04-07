#include "tests/framework/test_executor.hpp"

#include <chrono>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <iterator>
#include <optional>
#include <string>
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

// Capture declared output files from the work directory into produced_files.
// Called after backend execution, before the work directory is cleaned up.
void CaptureProducedFiles(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    CaseExecutionResult& result) {
  if (test_case.expected_files.empty() || work_directory.empty()) {
    return;
  }
  for (const auto& [filename, expected] : test_case.expected_files) {
    auto file_path = work_directory / filename;
    if (!std::filesystem::exists(file_path)) {
      // File not produced -- leave it absent; expectation eval will catch this
      continue;
    }
    std::ifstream input(file_path, std::ios::binary);
    if (!input.good()) {
      continue;
    }
    std::string content(
        (std::istreambuf_iterator<char>(input)),
        std::istreambuf_iterator<char>());
    // Normalize line endings
    std::erase(content, '\r');
    result.artifacts.produced_files[filename] = std::move(content);
  }
}

}  // namespace

auto ExecuteTestCase(
    const TestCase& test_case, BackendKind backend, bool force_two_state,
    std::chrono::seconds timeout) -> CaseExecutionResult {
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

    CaseExecutionResult result;
    switch (backend) {
      case BackendKind::kJit:
        result = RunJitBackend(test_case, work_directory, force_two_state);
        break;
      case BackendKind::kAot:
        result = RunAotBackend(test_case, work_directory, timeout);
        break;
      case BackendKind::kLli:
        result = RunLliBackend(test_case, work_directory, timeout);
        break;
    }

    // Capture produced files before the work directory is cleaned up.
    // This ensures file artifacts survive the fork boundary.
    if (result.execution.outcome == ExecutionOutcome::kSuccess) {
      CaptureProducedFiles(test_case, work_directory, result);
    }

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
