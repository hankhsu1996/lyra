#pragma once

#include <cstdint>
#include <functional>

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
class Observable;
class RuntimeProcess;

// The runtime capability surface reachable from generated code. Every side
// effect an emitted body performs -- I/O, scheduler verbs, time queries,
// deferred check submission, fork spawn -- lands on a method here. The host
// boundary (BindDesign, Run, elaboration walks) is added by the concrete
// Runtime; a caller holding a `RuntimeEffects&` cannot reach it.
//
// Non-polymorphic base: Runtime is the sole concrete implementation and every
// entry resolves at compile time. Inheritance carries the type-level access
// split without a vtable.
class RuntimeEffects {
 public:
  auto Stream() -> StreamDispatcher&;
  auto Diagnostic() -> DiagnosticDispatcher&;
  auto Files() -> FileTable&;
  auto PlusArgs() -> PlusArgsSource&;

  // LRM 16.3: record one evaluation of an immediate cover statement, and
  // whether that evaluation succeeded. `site` is the statement's own source
  // location, which is what separates one coverage goal from another.
  void RecordCoverage(const value::String& site, bool succeeded);

  void SubmitNba(std::function<void()> closure);
  void SubmitPostponed(std::function<void()> closure);
  // LRM 12.4.2.1: schedule a violation report to mature in the Observed region
  // of the current time slot. The report is pending on behalf of the process
  // that raised it, so a flush point that process reaches discards it; a check
  // reached before any process exists has no owner and nothing can discard it.
  void SubmitObserved(std::function<void()> fn);

  void TriggerValueChange(
      Observable& observable, const EdgeClassifier& classify);

  void ScheduleNextDelta(CoroutineHandle handle);
  void ScheduleInactive(CoroutineHandle handle);
  void ScheduleAtTime(SimTime when, CoroutineHandle handle);
  // LRM 20.2 / 20.10: `fatal=true` bumps the eventual `Run()` return to a
  // non-zero exit code.
  void RequestFinish(int level, bool fatal = false);
  // Adopts `coroutine` as a spawned child of the executing process's
  // lineage (LRM 9.5) and schedules it.
  void Spawn(Coroutine<void> coroutine);

  // The process whose body is executing right now (LRM 9.5); the observation
  // target `wait fork` reads and a fork spawn parents its branch to. Throws
  // if no process is executing.
  [[nodiscard]] auto CurrentProcess() -> RuntimeProcess&;
  [[nodiscard]] auto HasCurrentProcess() const -> bool;
  // Nullable form of `CurrentProcess()`: returns the executing process, or
  // `nullptr` when no process is running -- a scope-agnostic foreign query
  // (e.g. `svGetScope` outside an imported subroutine, LRM 35.5.3) uses this
  // to report a null scope rather than fault.
  [[nodiscard]] auto TryCurrentProcess() -> RuntimeProcess*;

  [[nodiscard]] auto Now() const -> SimTime;
  [[nodiscard]] auto GlobalPrecisionPower() const -> std::int8_t;

  // LRM 20.4.3: the design-wide `$timeformat` state.
  [[nodiscard]] auto TimeFormat() const -> const value::TimeFormat&;
  void SetTimeFormat(
      const value::PackedArray& units_power,
      const value::PackedArray& precision, const value::String& suffix,
      const value::PackedArray& min_width);
  void ResetTimeFormat();

  RuntimeEffects(const RuntimeEffects&) = delete;
  auto operator=(const RuntimeEffects&) -> RuntimeEffects& = delete;
  RuntimeEffects(RuntimeEffects&&) = delete;
  auto operator=(RuntimeEffects&&) -> RuntimeEffects& = delete;

 protected:
  RuntimeEffects() = default;
  ~RuntimeEffects() = default;
};

// The `RuntimeEffects` view of the Runtime attached to this thread. Reached
// without a receiver, so every body kind -- module process, class method,
// package function, class static method -- takes one uniform runtime-access
// path.
//
// Lifetime is Runtime attachment: valid for the full lifetime of the owning
// Runtime on this thread, including elaboration phases, scheduler-dispatch
// windows, and any span in which the Runtime exists but no generated code is
// currently executing. Throws when read on a thread with no attached Runtime
// so a stray access surfaces as a located error rather than reading a stale
// handle.
[[nodiscard]] auto current_runtime() -> RuntimeEffects&;

// RAII publication of a `RuntimeEffects` view as the current thread's ambient
// runtime. Save-and-restore of any previously-published handle, so a nested
// construction (a test spawning a second Runtime while the first is still
// torn down) leaves the outer handle in place until the outer's own guard
// destructs.
class CurrentRuntimeGuard {
 public:
  explicit CurrentRuntimeGuard(RuntimeEffects& effects);
  ~CurrentRuntimeGuard();

  CurrentRuntimeGuard(const CurrentRuntimeGuard&) = delete;
  auto operator=(const CurrentRuntimeGuard&) -> CurrentRuntimeGuard& = delete;
  CurrentRuntimeGuard(CurrentRuntimeGuard&&) = delete;
  auto operator=(CurrentRuntimeGuard&&) -> CurrentRuntimeGuard& = delete;

 private:
  RuntimeEffects* previous_;
};

// Publication of `process` as this runtime's ambient execution identity for
// the extent of a resume (LRM 9.5). Save-and-restore, so a nested foreign call
// that re-enters generated code cannot lose the identity on the way back.
class ProcessExecutionGuard {
 public:
  ProcessExecutionGuard(RuntimeEffects& effects, RuntimeProcess& process);
  ~ProcessExecutionGuard();

  ProcessExecutionGuard(const ProcessExecutionGuard&) = delete;
  auto operator=(const ProcessExecutionGuard&)
      -> ProcessExecutionGuard& = delete;
  ProcessExecutionGuard(ProcessExecutionGuard&&) = delete;
  auto operator=(ProcessExecutionGuard&&) -> ProcessExecutionGuard& = delete;

 private:
  RuntimeEffects* effects_;
  RuntimeProcess* previous_process_;
};

}  // namespace lyra::runtime
