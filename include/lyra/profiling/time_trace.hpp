#pragma once

#include <expected>
#include <filesystem>
#include <string>
#include <string_view>
#include <type_traits>

#include "lyra/support/statistics.hpp"

namespace lyra::profiling {

// Where a run's time went, recorded when asked as one span per piece of work
// and written as a Chrome trace, one lane per thread. It is the profiler that
// writes clang's `-ftime-trace`, so LLVM's own spans land in the same lanes,
// nested under the unit whose code they were generating.
//
// A span shorter than `granularity_us` is dropped, so a design of many small
// pieces writes a trace of its slow ones. Starting it covers every thread the
// run goes on to use: a thread is enrolled by its first span.
void TimeTraceStart(unsigned granularity_us);
auto TimeTraceStarted() -> bool;

// Writes what every thread recorded. Called once, after every thread that
// recorded anything has finished.
auto TimeTraceWrite(const std::filesystem::path& path)
    -> std::expected<void, std::string>;

// One piece of work, for as long as the scope lives. What the work is applied
// to -- a unit, a scope, a function -- is named by `detail`, which is called
// only when the run is traced, so naming costs nothing otherwise.
class TimeTraceScope {
 public:
  explicit TimeTraceScope(std::string_view name);

  template <typename Detail>
    requires std::is_invocable_v<Detail>
  TimeTraceScope(std::string_view name, Detail detail) {
    if (TimeTraceStarted()) {
      Begin(name, detail());
    }
  }

  ~TimeTraceScope();

  TimeTraceScope(const TimeTraceScope&) = delete;
  auto operator=(const TimeTraceScope&) -> TimeTraceScope& = delete;
  TimeTraceScope(TimeTraceScope&&) = delete;
  auto operator=(TimeTraceScope&&) -> TimeTraceScope& = delete;

 private:
  void Begin(std::string_view name, std::string detail);

  bool open_ = false;
};

// A stage that runs alone: a span, and the most memory the process held while
// it ran.
class StageScope {
 public:
  explicit StageScope(std::string_view stage) : span_(stage), memory_(stage) {
  }

 private:
  TimeTraceScope span_;
  support::StageMemory memory_;
};

}  // namespace lyra::profiling
