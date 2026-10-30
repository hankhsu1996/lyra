#pragma once

#include <expected>
#include <filesystem>
#include <string>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/verdict.hpp"

namespace conformance::framework {

// A tool a case is handed to: how it builds and runs the case's program in a
// directory, and how what it wrote is read. Each tool's reader is one, and a
// caller that checks more of a tool than the standard asks wraps one.
class Reader {
 public:
  Reader() = default;
  Reader(const Reader&) = delete;
  Reader(Reader&&) = delete;
  auto operator=(const Reader&) -> Reader& = delete;
  auto operator=(Reader&&) -> Reader& = delete;
  virtual ~Reader() = default;

  // Builds and runs `test_case` in `directory`, which is the reader's alone
  // for this case, and reads what the tool did.
  [[nodiscard]] virtual auto Observe(
      const Case& test_case, const std::filesystem::path& directory) const
      -> Observation = 0;
};

// What one case's run came to: what the tool was seen to do, and the verdict
// the standard gives it.
struct CaseResult {
  Observation seen;
  Verdict verdict;
};

// Runs one case on one tool and judges it. A program opens files where it
// starts, under names another case may use too, and a caller may run several
// cases at once, so each case runs in a directory of its own, removed once the
// run has been read. Nothing but a directory that could not be made keeps a
// case from a verdict.
auto RunOneCase(const Case& test_case, const Reader& reader)
    -> std::expected<CaseResult, std::string>;

}  // namespace conformance::framework
