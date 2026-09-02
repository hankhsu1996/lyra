#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace lyra::runtime {

// LRM 16.3 immediate cover results. Success of the expression is the goal a
// cover statement names, so what a run owes one is how many times it was
// evaluated and how many of those evaluations succeeded.
//
// A site is the statement's own source location. Two cover statements are two
// goals; one statement reached from two instances of its module is one goal
// here, because the code a specialization compiles is shared across its
// instances and nothing at the point of the call names which one reached it.
// Telling those apart is what the assertion API of LRM Clause 39 is for.
class CoverageLog {
 public:
  void Record(std::string_view site, bool succeeded);

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

  std::unordered_map<std::string, SiteCounts> sites_;
};

}  // namespace lyra::runtime
