#pragma once

#include <cstddef>
#include <cstdint>
#include <deque>
#include <functional>
#include <map>
#include <memory>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/format.hpp"

namespace lyra::runtime {

class Observable;
class RuntimeProcess;

enum class SchedulerPhase : std::uint8_t {
  kIdle,
  kActive,
  kInactive,
  kFlushUpdates,
  kCommitNba,
  kObserved,
  kReactive,
  kPostponed,
  kAdvanceTime,
};

struct EngineOptions {
  StreamDispatcher::StreamSink stream_sink;
  DiagnosticDispatcher::DiagnosticSink diagnostic_sink;
};

[[nodiscard]] auto DefaultEngineOptions() -> EngineOptions;

// One top-level block to install under $root. The scope object the emitted
// program constructs for it already carries its instance name.
struct TopBinding {
  Scope* scope;
};

class Engine {
 public:
  Engine();
  explicit Engine(EngineOptions options);

  // services_ holds a pointer into stream_; copying or moving Engine would
  // dangle that pointer.
  Engine(const Engine&) = delete;
  auto operator=(const Engine&) -> Engine& = delete;
  Engine(Engine&&) = delete;
  auto operator=(Engine&&) -> Engine& = delete;
  ~Engine() = default;

  void BindDesign(std::span<const TopBinding> tops);
  auto Run() -> int;

  auto Stream() -> StreamDispatcher& {
    return stream_;
  }

  auto Services() -> RuntimeServices& {
    return services_;
  }

  void SubmitNba(std::function<void()> closure);
  void SubmitPostponed(std::function<void()> closure);

  void TriggerValueChange(
      Observable& observable, const EdgeClassifier& classify);

  // Generic primitives exposed to runtime data types and awaitables so they
  // can manage their own producer/consumer wiring without the engine needing
  // to know about them. Each awaitable does its scheduling work in
  // `await_suspend` by reaching back through `RuntimeServices` -- the engine
  // never inspects what kind of wait a process is in.
  //
  // ScheduleNextDelta   -- enqueue on the next delta of the current slot
  //                        (used by event triggers, value-change wakeups,
  //                        anything that wants "wake on next opportunity").
  // ScheduleInactive    -- enqueue on the inactive region of the current
  //                        slot (used by `#0` delays).
  // ScheduleAtTime      -- enqueue at a future SimTime (used by `#N`).
  // RequestFinish       -- tear down the simulation (used by `$finish`).
  // Now                 -- query current simulation time.
  // Spawn               -- adopt a coroutine created mid-simulation as a
  //                        runnable process and schedule it (used by fork-join
  //                        branches); construct-neutral by design.
  void ScheduleNextDelta(CoroutineHandle handle);
  void ScheduleInactive(CoroutineHandle handle);
  void ScheduleAtTime(SimTime when, CoroutineHandle handle);
  void RequestFinish(int level);
  void Spawn(Coroutine coroutine);
  [[nodiscard]] auto Now() const -> SimTime {
    return now_;
  }
  [[nodiscard]] auto GlobalPrecisionPower() const -> std::int8_t {
    return global_precision_power_;
  }

  // LRM 20.4.3: the design-wide `$timeformat` settings that drive every `%t`.
  [[nodiscard]] auto TimeFormat() const -> const value::TimeFormat& {
    return time_format_;
  }
  void SetTimeFormat(
      std::int8_t units_power, std::int32_t precision, std::string suffix,
      std::int32_t min_width) {
    time_format_ = value::TimeFormat{
        .units_power = units_power,
        .precision = precision,
        .suffix = std::move(suffix),
        .min_width = min_width};
  }
  // LRM Table 20-3: the no-argument `$timeformat` form restores the defaults --
  // display unit is the design-global precision, precision 0, no suffix, field
  // width 20.
  void ResetTimeFormat() {
    time_format_ = value::TimeFormat{};
    time_format_.units_power = global_precision_power_;
  }

 private:
  struct SchedulerQueues {
    std::deque<CoroutineHandle> active;
    std::deque<CoroutineHandle> inactive;
    std::vector<CoroutineHandle> next_delta;
    std::vector<std::function<void()>> nba;
    std::vector<std::function<void()>> postponed;
    std::map<SimTime, std::vector<CoroutineHandle>> delayed;
    std::vector<CoroutineHandle> finals;
  };

  static constexpr std::size_t kMaxCurrentTimeIterations = 10000;
  static constexpr std::size_t kMaxDeltaCyclesPerTimeSlot = 10000;

  void EnsureReadyToRun();
  // Fixes the design-global time precision (LRM 3.14.3) as the minimum declared
  // precision across the bound scope tree. The simulation tick; delays scale to
  // it.
  void ResolveGlobalTimePrecision();
  void RegisterProcesses();
  void RunProcess(CoroutineHandle handle);
  void DrainRunnableQueue(std::deque<CoroutineHandle>& queue);

  void ExecuteCurrentTimeSlot();
  void ExecuteActiveRegion();
  void ExecuteInactiveRegion();
  void FlushRuntimeUpdates();
  void ExecuteNbaRegion();
  void ExecuteObservedRegion();
  void ExecuteReactiveRegion();
  void ExecutePostponedRegion();
  void ExecuteFinalProcesses();

  void AdvanceToNextTime();
  void AdvanceDeltaCycle();
  void PromoteNextDeltaToActive();

  [[nodiscard]] auto HasCurrentTimeWork() const -> bool;
  [[nodiscard]] auto HasFutureTimedWork() const -> bool;
  [[nodiscard]] auto HasScheduledWork() const -> bool;
  [[nodiscard]] auto HasNextDeltaWork() const -> bool;
  [[nodiscard]] auto IsRunnablePhase() const -> bool;

  void ScheduleActive(CoroutineHandle handle);

  [[nodiscard]] static auto CheckedAdd(SimTime base, SimDuration delta)
      -> SimTime;

  StreamDispatcher stream_;
  DiagnosticDispatcher diagnostic_;
  FileTable files_;
  RuntimeServices services_{stream_, diagnostic_, files_, *this};
  std::unique_ptr<Scope> root_;
  // Processes created during simulation (fork-join branches), owned for their
  // dynamic lifetime. Each is dropped in RunProcess when it completes; static
  // processes live on their scope instead.
  std::vector<std::unique_ptr<RuntimeProcess>> spawned_;
  SchedulerQueues queues_;
  SimTime now_ = 0;
  std::int8_t global_precision_power_ = kDefaultTimePrecisionPower;
  value::TimeFormat time_format_;
  SchedulerPhase phase_ = SchedulerPhase::kIdle;
  std::size_t current_delta_ = 0;
  bool bound_ = false;
  bool ran_ = false;
  bool finished_ = false;
};

}  // namespace lyra::runtime
