#pragma once

#include <string>
#include <variant>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"

namespace conformance::framework {

// What one run says of one tool against one requirement. A tool that cannot be
// asked for what the case needs, or a harness that could not run it, has not
// been judged at all; an observation that cannot decide the claim is said to,
// and is never taken for an answer either way.
struct Conforms {};

struct DoesNotConform {
  std::string why;
};

struct Unresolved {
  std::string why;
};

struct NotRun {
  std::string why;
};

using Verdict = std::variant<Conforms, DoesNotConform, Unresolved, NotRun>;

// The verdict a claim and an observation give, the same for every tool.
auto Judge(const Claim& claim, const Observation& observation) -> Verdict;

// The verdict a whole case gets: its claim's, and where that conforms, each
// file the case expects compared with the one the run left, as the standard
// reads that kind of file. A file the run did not write does not conform.
auto JudgeCase(const Case& test_case, const Observation& observation)
    -> Verdict;

// The verdict in words: "conforms", "does not conform: ...".
auto Describe(const Verdict& verdict) -> std::string;

}  // namespace conformance::framework
