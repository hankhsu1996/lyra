#include "lyra/runtime/coverage.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/source_location.hpp"
#include "lyra/runtime/diagnostic.hpp"

namespace lyra::runtime {

void CoverageLog::Record(base::SourceLocation site, bool succeeded) {
  auto entry =
      sites_
          .try_emplace(
              site.Identity(),
              SiteCounts{
                  .evaluated = 0, .succeeded = 0, .arrival = sites_.size()})
          .first;
  ++entry->second.evaluated;
  if (succeeded) {
    ++entry->second.succeeded;
  }
}

auto CoverageLog::Report() const -> std::vector<std::string> {
  // Two units each state a constant of their own for one statement, so sites
  // naming one place are one goal, reached when the first of them was.
  std::map<std::string, SiteCounts> goals;
  for (const auto& [site, counts] : sites_) {
    const auto [goal, is_new] = goals.try_emplace(
        SourceLocationText(base::SourceLocation(site)), counts);
    if (!is_new) {
      goal->second.evaluated += counts.evaluated;
      goal->second.succeeded += counts.succeeded;
      goal->second.arrival = std::min(goal->second.arrival, counts.arrival);
    }
  }
  std::vector<std::pair<std::size_t, std::string>> ordered;
  ordered.reserve(goals.size());
  for (const auto& [place, counts] : goals) {
    ordered.emplace_back(
        counts.arrival, std::format(
                            "{}: cover evaluated {}, succeeded {}", place,
                            counts.evaluated, counts.succeeded));
  }
  std::ranges::sort(ordered);

  std::vector<std::string> lines;
  lines.reserve(ordered.size());
  for (auto& entry : ordered) {
    lines.push_back(std::move(entry.second));
  }
  return lines;
}

}  // namespace lyra::runtime
