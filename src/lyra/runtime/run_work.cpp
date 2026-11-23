#include "lyra/runtime/run_work.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/region.hpp"

namespace lyra::runtime {

namespace {

constexpr std::string_view kStatisticsOption = "lyra+stats=";

// LRM 4.4's name for each region.
auto RegionName(support::Region region) -> std::string_view {
  switch (region) {
    case support::Region::kPreponed:
      return "Preponed";
    case support::Region::kPreActive:
      return "Pre-Active";
    case support::Region::kActive:
      return "Active";
    case support::Region::kInactive:
      return "Inactive";
    case support::Region::kPreNba:
      return "Pre-NBA";
    case support::Region::kNba:
      return "NBA";
    case support::Region::kPostNba:
      return "Post-NBA";
    case support::Region::kObserved:
      return "Observed";
    case support::Region::kReactive:
      return "Reactive";
    case support::Region::kReInactive:
      return "Re-Inactive";
    case support::Region::kReNba:
      return "Re-NBA";
    case support::Region::kPrePostponed:
      return "Pre-Postponed";
    case support::Region::kPostponed:
      return "Postponed";
  }
  throw InternalError("RegionName: unknown region");
}

struct Count {
  std::string kind;
  std::uint64_t count = 0;
};

auto CountsOf(const RunWork& work) -> std::vector<Count> {
  std::vector<Count> counts{{.kind = "time slots", .count = work.time_slots}};
  for (std::size_t index = 0; index < work.regions.size(); ++index) {
    const std::string_view region =
        RegionName(static_cast<support::Region>(index));
    const RegionWork& done = work.regions.at(index);
    counts.push_back(
        {.kind = std::format("{} passes", region), .count = done.passes});
    counts.push_back(
        {.kind = std::format("{} activations run", region),
         .count = done.activations_run});
    counts.push_back(
        {.kind = std::format("{} activations run again", region),
         .count = done.activations_run_again});
    counts.push_back(
        {.kind = std::format("{} deferred effects run", region),
         .count = done.deferred_effects_run});
    counts.push_back(
        {.kind = std::format("{} timers fired", region),
         .count = done.timers_fired});
  }
  counts.push_back(
      {.kind = "activations run at the occurrence",
       .count = work.activations_run_at_the_occurrence});
  counts.push_back({.kind = "changes told", .count = work.changes_told});
  counts.push_back(
      {.kind = "memberships reached", .count = work.memberships_reached});
  counts.push_back(
      {.kind = "memberships passed over",
       .count = work.memberships_passed_over});
  counts.push_back(
      {.kind = "activations woken", .count = work.activations_woken});
  return counts;
}

}  // namespace

void KeptWork::PassBegins(support::Region region) {
  ++WorkOf(region).passes;
  if (executing != region) {
    executing = region;
    ran_in_execution.clear();
  }
}

auto KeptWork::RunsAgain(const Activation& activation) -> bool {
  return !ran_in_execution.insert(&activation).second;
}

void KeptWork::Reached(bool unaffected) {
  ++counts.memberships_reached;
  if (unaffected) {
    ++counts.memberships_passed_over;
  }
}

auto KeptWork::WorkOf(support::Region region) -> RegionWork& {
  return counts.regions.at(static_cast<std::size_t>(region));
}

auto TheKeptWork() -> KeptWork& {
  static KeptWork kept;
  return kept;
}

auto RunStatisticsFile(std::span<const std::string> plusargs)
    -> std::optional<std::string> {
  std::optional<std::string> file;
  for (const std::string& argument : plusargs) {
    if (std::string_view{argument}.starts_with(kStatisticsOption)) {
      file = argument.substr(kStatisticsOption.size());
    }
  }
  return file;
}

// Every kind is named by this file in plain text, so none needs escaping and
// the object is written as it reads.
auto RunStatisticsText(const RunWork& work) -> std::string {
  std::string members;
  for (const Count& one : CountsOf(work)) {
    members += std::format(
        "{}    \"{}\": {}", members.empty() ? "" : ",\n", one.kind, one.count);
  }
  return std::format("{{\n  \"work\": {{\n{}\n  }}\n}}\n", members);
}

}  // namespace lyra::runtime
