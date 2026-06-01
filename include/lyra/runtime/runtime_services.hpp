#pragma once

#include <functional>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

class StreamDispatcher;
class DiagnosticDispatcher;
class Engine;
class Observable;
class RuntimeProcess;

class RuntimeServices {
 public:
  RuntimeServices(
      StreamDispatcher& stream, DiagnosticDispatcher& diagnostic,
      Engine& engine)
      : stream_(&stream), diagnostic_(&diagnostic), engine_(&engine) {
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

  void SubmitNba(std::function<void()> closure);

  void TriggerValueChange(
      Observable& observable, const EdgeClassifier& classify);

  // Generic forwarders to Engine primitives used by runtime data types
  // (NamedEvent, etc.) and by every awaitable's `await_suspend` so they
  // manage their own producer/consumer wiring without going through
  // engine-side per-feature code paths.
  //
  // ScheduleProcess     -- enqueue on next delta (event triggers, value
  //                        change wakeups).
  // ScheduleInactive    -- enqueue on the inactive region of this slot
  //                        (`#0` delay).
  // ScheduleAtTime      -- enqueue at a future SimTime (`#N` delay).
  // RequestFinish       -- mark simulation to stop after the current slot
  //                        completes (`$finish`).
  // Now                 -- current simulation time.
  void ScheduleProcess(RuntimeProcess& process);
  void ScheduleInactive(RuntimeProcess& process);
  void ScheduleAtTime(SimTime when, RuntimeProcess& process);
  void RequestFinish(int level);
  [[nodiscard]] auto Now() const -> SimTime;

 private:
  StreamDispatcher* stream_ = nullptr;
  DiagnosticDispatcher* diagnostic_ = nullptr;
  Engine* engine_ = nullptr;
};

}  // namespace lyra::runtime
