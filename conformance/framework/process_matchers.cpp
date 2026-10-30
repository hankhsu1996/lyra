#include "conformance/framework/process_matchers.hpp"

#include <gmock/gmock.h>
#include <optional>
#include <ostream>
#include <utility>

#include "conformance/framework/process.hpp"

namespace conformance::framework {

void PrintTo(const ProcessOutcome& outcome, std::ostream* out) {
  *out << "a child that " << Describe(outcome.end);
}

namespace {

class ExitedWithCodeMatcher {
 public:
  using is_gtest_matcher = void;

  explicit ExitedWithCodeMatcher(testing::Matcher<int> code)
      : code_(std::move(code)) {
  }

  auto MatchAndExplain(
      const ProcessOutcome& outcome,
      testing::MatchResultListener* listener) const -> bool {
    const std::optional<int> code = ExitCodeOf(outcome);
    return code && testing::ExplainMatchResult(code_, *code, listener);
  }

  void DescribeTo(std::ostream* out) const {
    *out << "exited with a code that ";
    code_.DescribeTo(out);
  }

  void DescribeNegationTo(std::ostream* out) const {
    *out << "did not exit with a code that ";
    code_.DescribeTo(out);
  }

 private:
  testing::Matcher<int> code_;
};

}  // namespace

auto ExitedWithCode(const testing::Matcher<int>& code)
    -> testing::Matcher<const ProcessOutcome&> {
  return ExitedWithCodeMatcher(code);
}

}  // namespace conformance::framework
