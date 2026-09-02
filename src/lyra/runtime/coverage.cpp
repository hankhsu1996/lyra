#include "lyra/runtime/coverage.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace lyra::runtime {

void CoverageLog::Record(std::string_view site, bool succeeded) {
  auto entry =
      sites_
          .try_emplace(
              std::string(site),
              SiteCounts{
                  .evaluated = 0, .succeeded = 0, .arrival = sites_.size()})
          .first;
  ++entry->second.evaluated;
  if (succeeded) {
    ++entry->second.succeeded;
  }
}

auto CoverageLog::Report() const -> std::vector<std::string> {
  std::vector<std::pair<std::size_t, std::string>> ordered;
  ordered.reserve(sites_.size());
  for (const auto& [site, counts] : sites_) {
    ordered.emplace_back(
        counts.arrival, std::format(
                            "{}: cover evaluated {}, succeeded {}", site,
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
