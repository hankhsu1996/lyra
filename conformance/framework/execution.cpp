#include "conformance/framework/execution.hpp"

#include <chrono>
#include <filesystem>
#include <format>
#include <optional>
#include <system_error>
#include <utility>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/process.hpp"
#include "conformance/framework/verdict.hpp"

namespace conformance::framework {

auto RunOneCase(
    const Case& test_case, const Reader& reader,
    std::optional<std::chrono::seconds> time_limit) -> CaseResult {
  const auto began = std::chrono::steady_clock::now();
  std::optional<std::chrono::steady_clock::time_point> deadline;
  if (time_limit) {
    deadline = began + *time_limit;
  }
  Observation seen;
  if (const auto here = MakeScratchDir(); !here) {
    seen.end = HarnessFailed{
        std::format("no directory to run the case in: {}", here.error())};
  } else {
    seen = reader.Observe(test_case, *here, deadline);
    std::error_code ignored;
    std::filesystem::remove_all(*here, ignored);
  }
  Verdict verdict = JudgeCase(test_case, seen);
  return CaseResult{
      .seen = std::move(seen),
      .verdict = std::move(verdict),
      .took = std::chrono::steady_clock::now() - began};
}

}  // namespace conformance::framework
