#pragma once

#include <cstdint>
#include <functional>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class StreamDispatcher;
class DiagnosticDispatcher;
class FileTable;
class PlusArgsSource;
class Engine;
class Observable;

class RuntimeServices {
 public:
  RuntimeServices(
      StreamDispatcher& stream, DiagnosticDispatcher& diagnostic,
      FileTable& files, PlusArgsSource& plusargs, Engine& engine)
      : stream_(&stream),
        diagnostic_(&diagnostic),
        files_(&files),
        plusargs_(&plusargs),
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

  auto PlusArgs() -> PlusArgsSource& {
    if (plusargs_ == nullptr) {
      throw InternalError("RuntimeServices has no PlusArgsSource");
    }
    return *plusargs_;
  }

  void SubmitNba(std::function<void(RuntimeServices&)> closure);
  void SubmitPostponed(std::function<void()> closure);

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
  //                        completes (`$finish` and the implicit shutdown
  //                        from `$fatal`; `fatal` true makes the eventual
  //                        Run() return a non-zero exit code per LRM 20.10).
  // Now                 -- current simulation time.
  //
  // The scheduled unit is the coroutine frame that suspended (the innermost one
  // when a task enabled by the process is suspended), so resuming it re-enters
  // exactly that suspension point.
  void ScheduleNextDelta(CoroutineHandle handle);
  void ScheduleInactive(CoroutineHandle handle);
  void ScheduleAtTime(SimTime when, CoroutineHandle handle);
  void RequestFinish(int level, bool fatal = false);
  void Spawn(Coroutine<void> coroutine);
  [[nodiscard]] auto Now() const -> SimTime;

  // The design-global time precision (LRM 3.14.3) the delay awaitable scales a
  // scope-precision delay against to reach the engine's tick.
  [[nodiscard]] auto GlobalPrecisionPower() const -> std::int8_t;

  // The design-wide `$timeformat` state (LRM 20.4.3) read by `%t`. The reader
  // is the value-format machinery; the two setter methods are the
  // `$timeformat` SV surface -- the four-argument set form takes the LRM
  // 20.4.3 display arguments as SV values, and the no-argument reset form
  // restores the LRM Table 20-3 defaults (display unit is the design-global
  // precision, which only the runtime resolves).
  [[nodiscard]] auto TimeFormat() const -> const value::TimeFormat&;
  void SetTimeFormat(
      const value::PackedArray& units_power,
      const value::PackedArray& precision, const value::String& suffix,
      const value::PackedArray& min_width);
  void ResetTimeFormat();

 private:
  StreamDispatcher* stream_ = nullptr;
  DiagnosticDispatcher* diagnostic_ = nullptr;
  FileTable* files_ = nullptr;
  PlusArgsSource* plusargs_ = nullptr;
  Engine* engine_ = nullptr;
};

}  // namespace lyra::runtime
