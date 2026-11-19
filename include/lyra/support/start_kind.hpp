#pragma once

#include <cstdint>

namespace lyra::support {

// The kinds of static process an instance starts at time zero (LRM 9.2, 10.3,
// Annex P). Shared vocabulary: the compiler states the kind of each process it
// hands the run, and the runtime reads it for when the process starts and for
// what created its thread, which decides whether process control may act on it
// (LRM 9.7). The runtime owns the order they start in.
enum class StartKind : std::uint8_t {
  kAlwaysProcedure,
  // An execution of the scope that is no procedure and runs inside each
  // occurrence of what it waits for: a clocking block's sampling (LRM 14.13).
  kObserver,
  kContinuousDriver,
  kInitialProcedure,
  // An `always_comb` or `always_latch` (LRM 9.2.2.2, 9.2.2.3).
  kTriggeredProcedure,
};

// How the run calls the parts of a static process whose body runs to its end
// each time it is triggered. Running its loop's statements and then stopping,
// it runs a pass as it starts (a continuous assignment, LRM 10.3); stopping and
// then running them, it waits first (an `always` whose event control opens its
// statement, LRM 9.4.2); a body that never stops runs once and ends.
enum class ReactionForm : std::uint8_t {
  kRunThenStop,
  kStopThenRun,
  kRunsOnce,
};

}  // namespace lyra::support
