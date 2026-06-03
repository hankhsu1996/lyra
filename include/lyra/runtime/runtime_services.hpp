#pragma once

#include <functional>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

class StreamDispatcher;
class DiagnosticDispatcher;
class FileTable;
class Engine;
class Observable;

class RuntimeServices {
 public:
  RuntimeServices(
      StreamDispatcher& stream, DiagnosticDispatcher& diagnostic,
      FileTable& files, Engine& engine)
      : stream_(&stream),
        diagnostic_(&diagnostic),
        files_(&files),
        engine_(&engine) {
  }

  auto Stream() -> StreamDispatcher& {
    if (stream_ == nullptr) {
      throw InternalError("RuntimeServices has no StreamDispatcher");
    }
    return *stream_;
  }

  auto Diagnostic() -> DiagnosticDispatcher& {
    if (diagnostic_ == nullptr) {
      throw InternalError("RuntimeServices has no DiagnosticDispatcher");
    }
    return *diagnostic_;
  }

  auto Files() -> FileTable& {
    if (files_ == nullptr) {
      throw InternalError("RuntimeServices has no FileTable");
    }
    return *files_;
  }

  void SubmitNba(std::function<void()> closure);

  void TriggerValueChange(
      Observable& observable, const EdgeClassifier& classify);

  // Generic forwarders to Engine primitives used by runtime data types
  // (NamedEvent, etc.) and by every awaitable's `await_suspend` so they
  // manage their own producer/consumer wiring without going through
  // engine-side per-feature code paths.
  //
  // ScheduleNextDelta   -- enqueue on next delta (event triggers, value
  //                        change wakeups).
  // ScheduleInactive    -- enqueue on the inactive region of this slot
  //                        (`#0` delay).
  // ScheduleAtTime      -- enqueue at a future SimTime (`#N` delay).
  // RequestFinish       -- mark simulation to stop after the current slot
  //                        completes (`$finish`).
  // Now                 -- current simulation time.
  //
  // The scheduled unit is the coroutine frame that suspended (the innermost one
  // when a task enabled by the process is suspended), so resuming it re-enters
  // exactly that suspension point.
  void ScheduleNextDelta(CoroutineHandle handle);
  void ScheduleInactive(CoroutineHandle handle);
  void ScheduleAtTime(SimTime when, CoroutineHandle handle);
  void RequestFinish(int level);
  [[nodiscard]] auto Now() const -> SimTime;

  // The design-global time precision (LRM 3.14.3) the delay awaitable scales a
  // scope-precision delay against to reach the engine's tick.
  [[nodiscard]] auto GlobalPrecisionPower() const -> std::int8_t;

 private:
  StreamDispatcher* stream_ = nullptr;
  DiagnosticDispatcher* diagnostic_ = nullptr;
  FileTable* files_ = nullptr;
  Engine* engine_ = nullptr;
};

}  // namespace lyra::runtime
