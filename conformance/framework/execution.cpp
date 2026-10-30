#include "conformance/framework/execution.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <string>
#include <system_error>
#include <utility>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/process.hpp"
#include "conformance/framework/verdict.hpp"

namespace conformance::framework {

auto RunOneCase(const Case& test_case, const Reader& reader)
    -> std::expected<CaseResult, std::string> {
  const auto here = MakeScratchDir();
  if (!here) {
    return std::unexpected(
        std::format("no directory to run the case in: {}", here.error()));
  }
  Observation seen = reader.Observe(test_case, *here);
  Verdict verdict = JudgeCase(test_case, seen);
  std::error_code ignored;
  std::filesystem::remove_all(*here, ignored);
  return CaseResult{.seen = std::move(seen), .verdict = std::move(verdict)};
}

}  // namespace conformance::framework
