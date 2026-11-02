#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/base/source_location.hpp"

namespace lyra::runtime {

// LRM 16.3 immediate cover results. Success of the expression is the goal a
// cover statement names, so what a run owes one is how many times it was
// evaluated and how many of those evaluations succeeded.
//
// A site is the statement's own source location. Two cover statements are two
// goals; one statement reached from two instances of its module is one goal
// here, whether or not the two share code, because nothing at the point of the
// call names which instance reached it. Telling those apart is what the
// assertion API of LRM Clause 39 is for.
//
// Evaluating a statement counts it against the address of its constant, so no
// text is made until the report.
class CoverageLog {
 public:
  void Record(base::SourceLocation site, bool succeeded);

  // One line per site, in the order the sites were first reached -- the only
  // order there is, because a site enters the log by being evaluated and
  // nothing declares one ahead of that.
  [[nodiscard]] auto Report() const -> std::vector<std::string>;

 private:
  struct SiteCounts {
    std::uint64_t evaluated;
    std::uint64_t succeeded;
    std::size_t arrival;
  };

  std::unordered_map<const base::SourceLocation::Impl*, SiteCounts> sites_;
};

}  // namespace lyra::runtime
