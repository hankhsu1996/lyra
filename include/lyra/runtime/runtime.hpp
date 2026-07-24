#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/value/format.hpp"

namespace lyra::runtime {

class Design;
class Observable;
class RuntimeProcess;
class Scope;

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

struct RuntimeOptions {
  StreamDispatcher::StreamSink stream_sink;
  DiagnosticDispatcher::DiagnosticSink diagnostic_sink;
  // LRM 21.6 command-line plusarg tokens with the `+` prefix already
  // stripped, in the order they appeared on the host command line.
  std::vector<std::string> plusargs;
};

[[nodiscard]] auto DefaultRuntimeOptions() -> RuntimeOptions;

// The concrete simulation runtime. Owns every mutable piece of simulator
// state -- time, region queues, execution ambient, I/O sinks, the attached
// design, the process registry -- and drives the elaboration walks and the
// region loop. Generated code sees only the `RuntimeEffects` view; the host
// boundary here (BindDesign, Run) is not visible through that view.
class Runtime final : public RuntimeEffects {
 public:
  Runtime();
  explicit Runtime(RuntimeOptions options);

  Runtime(const Runtime&) = delete;
  auto operator=(const Runtime&) -> Runtime& = delete;
  Runtime(Runtime&&) = delete;
  auto operator=(Runtime&&) -> Runtime& = delete;
  ~Runtime();

  // Takes ownership of the elaborated `design`, then walks its scope tree in
  // three top-down passes (resolve state, initialize state, create processes).
  // Each per-scope call runs inside a `ScopeExecutionGuard` so a runtime effect
  // the body reaches (e.g. a deferred check emitted by a variable initializer)
  // attributes to the scope currently being walked. Design-wide barrier per
  // phase: every scope resolves before any initializes; every scope
  // initializes before any activates.
  void BindDesign(std::unique_ptr<Design> design);
  auto Run() -> int;

  // Push `process` onto the primary registry and the by-scope index in
  // lockstep. The by-scope index is a set of raw back-pointers keyed by the
  // owning scope; every hierarchical query (LRM 9.7 `disable`, LRM 9.6.1
  // `wait fork` descendant walk, scope teardown, `%m` attribution) reaches
  // it here. Called by process registration (RegisterInitial / Final free
  // functions) and by the fork spawn path. Public because the free
  // registration functions live outside this class.
  void RegisterProcessInRegistry(std::shared_ptr<RuntimeProcess> process);

 private:
  friend class RuntimeEffects;
  friend class CurrentRuntimeGuard;
  friend class ProcessExecutionGuard;
  friend class ScopeExecutionGuard;

  // An activation waiting on the runtime is parked on one of these exactly as
  // it parks on an event or an observable, so cancelling it while it is queued
  // is the same constant-time unlink -- the runtime is never searched.
  struct SchedulerQueues {
    RegistrationList active;
    RegistrationList inactive;
    RegistrationList next_delta;
    std::vector<std::function<void(RuntimeEffects&)>> nba;
    std::vector<std::function<void()>> postponed;
    // Per-scope pending observed closures (LRM 16.14.6). Keyed by the
    // attribution scope pointer; inner vector is indexed by site_id and
    // overwritten in place so a re-submit at the same (scope, site) in
    // the same slot last-writes-wins. Drained tree-order at Observed
    // region entry.
    std::unordered_map<Scope*, std::vector<std::function<void()>>> observed;
    std::map<SimTime, RegistrationList> delayed;
    RegistrationList finals;
    // The snapshot a region drain is working through, moved out of its source
    // queue so that work arriving mid-drain lands in the queue and waits for
    // the next pass (LRM 9.3.2).
    RegistrationList draining;
  };

  static constexpr std::size_t kMaxCurrentTimeIterations = 10000;
  static constexpr std::size_t kMaxDeltaCyclesPerTimeSlot = 10000;

  // Wait-resume verb: revoke the leaf's scheduler participation and park it
  // on the next-delta queue. LRM 9.3.2 delta cycle boundary.
  void EnqueueNextDelta(CoroutineHandle handle);

  void EnsureReadyToRun();
  // LRM 3.14.3: design-global tick is the minimum declared precision across
  // the tree.
  void ResolveGlobalTimePrecision();
  void RegisterProcesses();
  void WalkResolve(Scope& scope);
  void WalkInitialize(Scope& scope);
  void WalkActivate(Scope& scope);

  // Runs a suspended frame's owning process against this runtime's execution
  // ambient. On completion the terminal transition drains the process's own
  // `await` waiters into `woken` for the caller to schedule.
  auto ResumeProcess(
      CoroutineHandle handle, std::vector<CoroutineHandle>& woken) -> bool;
  void RunProcess(CoroutineHandle handle);
  void DrainRunnableQueue(RegistrationList& queue);

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
  FileTable files_{stream_};
  PlusArgsSource plusargs_;
  CurrentRuntimeGuard current_runtime_guard_{*this};
  std::unique_ptr<Design> design_;
  SchedulerQueues queues_;
  std::vector<std::shared_ptr<RuntimeProcess>> processes_;
  // Secondary index: pointers into `processes_` keyed by owning scope. Kept
  // in lockstep with `processes_` (register push, teardown erase). Consumers
  // that ask "which processes belong to this scope" (hierarchical `disable`,
  // `wait fork` descendant walk, scope teardown, `%m` attribution) reach it
  // here without scanning the full registry.
  std::unordered_map<Scope*, std::vector<RuntimeProcess*>> processes_by_scope_;
  RuntimeProcess* current_process_ = nullptr;
  Scope* current_scope_ = nullptr;
  SimTime now_ = 0;
  std::int8_t global_precision_power_ = kDefaultTimePrecisionPower;
  value::TimeFormat time_format_;
  SchedulerPhase phase_ = SchedulerPhase::kIdle;
  std::size_t current_delta_ = 0;
  bool bound_ = false;
  bool ran_ = false;
  bool finished_ = false;
  bool fatal_finish_ = false;
};

// Reached by generated `RegisterInitial` / `RegisterFinal` builtins: creates a
// process bound to `owning_scope` and registers it in the ambient runtime.
// LRM 9.2 lifecycle: an `initial` starts on the Active queue at time 0; a
// `final` parks on the finals list until shutdown. The scope handle arrives
// as a pointer because the generated call site is holding the `self` pointer
// from its enclosing body.
void RegisterInitialProcess(Scope* owning_scope, Coroutine<void> coroutine);
void RegisterFinalProcess(Scope* owning_scope, Coroutine<void> coroutine);

}  // namespace lyra::runtime
