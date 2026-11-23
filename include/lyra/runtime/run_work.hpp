#pragma once

#include <array>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <unordered_set>

#include "lyra/support/region.hpp"

namespace lyra::runtime {

struct Activation;

// What the passes of one region did over the whole run.
struct RegionWork {
  std::uint64_t passes = 0;
  std::uint64_t activations_run = 0;
  // Runs of an activation that had already run in the same execution of the
  // region -- its passes since a pass of another region last ran (LRM 4.5's
  // execute_region): the runs an order of the region's work could have saved.
  std::uint64_t activations_run_again = 0;
  std::uint64_t deferred_effects_run = 0;
  std::uint64_t timers_fired = 0;
};

// How many times the run did each kind of scheduling work, counted where the
// work is done, as a compiler counts its own. Only the build of the runtime
// shipped for counting counts anything: a count is work on every event, which a
// run nobody asked to count would otherwise pay for.
struct RunWork {
  std::uint64_t time_slots = 0;
  std::array<RegionWork, support::kRegionCount> regions{};
  std::uint64_t activations_run_at_the_occurrence = 0;
  // Each occurrence told to the waits it reaches: a write of a place, an event
  // triggered, an object expiring, the net change an interval left.
  std::uint64_t changes_told = 0;
  std::uint64_t memberships_reached = 0;
  // Memberships a change reached whose part the change shows unaffected.
  std::uint64_t memberships_passed_over = 0;
  std::uint64_t activations_woken = 0;
};

// What a counting run keeps: its counts, and the region whose execution is
// under way with the activations that have run in it. The process holds one,
// as a compiler holds its statistics, because an activation ending has to
// leave it and reaches no run to do so.
struct KeptWork {
  RunWork counts;
  std::optional<support::Region> executing;
  std::unordered_set<const Activation*> ran_in_execution;

  // A pass of `region` begins, which begins an execution of it unless the
  // last pass was of it too.
  void PassBegins(support::Region region);
  // `activation` runs in the pass under way, answering whether it already ran
  // in this execution of the region.
  auto RunsAgain(const Activation& activation) -> bool;
  // A membership a change reached, which the change may show unaffected.
  void Reached(bool unaffected);
  [[nodiscard]] auto WorkOf(support::Region region) -> RegionWork&;
};

[[nodiscard]] auto TheKeptWork() -> KeptWork&;

// The file the run's options ask its statistics to be written to, the last of
// them where several do.
[[nodiscard]] auto RunStatisticsFile(std::span<const std::string> plusargs)
    -> std::optional<std::string>;

// The statistics as a run writes them: its work, each kind by name, read as the
// compiler's own statistics are read.
[[nodiscard]] auto RunStatisticsText(const RunWork& work) -> std::string;

}  // namespace lyra::runtime
