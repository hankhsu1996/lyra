#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/coverage.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/mem_file.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/runtime/time_slot.hpp"
#include "lyra/value/format.hpp"

namespace lyra::runtime {

class Design;
class Observable;
class RuntimeProcess;
class Scope;

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
  // Design-wide barrier per phase: every scope resolves before any
  // initializes; every scope initializes before any activates.
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

  // How many times one slot may re-enter its regions before the design is
  // declared unable to settle. A design that keeps scheduling work at the
  // current time never advances, so this bound is what turns a run that would
  // not end into one that reports why.
  static constexpr std::size_t kMaxRegionPassesPerSlot = 10000;

  // The slot at `when`, created on first use. LRM 4.4 has the simulator never
  // go backwards in time, so work placed in a slot earlier than the current one
  // would be lost with nothing to show for it, which is why naming one faults
  // here instead.
  auto SlotAt(SimTime when) -> TimeSlot&;

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

  // LRM 4.5: Preponed, then regions taken in order until nothing in Active
  // through Re-NBA remains, then Postponed. Work a region produces lands back
  // in the slot, so the middle step repeats until the slot settles.
  void ExecuteTimeSlot(TimeSlot& slot);
  void RunRegion(TimeSlot& slot, Region region);
  void ExecuteFinalProcesses();
  // LRM 16.3 requires a tool to report immediate cover results at the end of
  // simulation where it offers no assertion API to ask for them on demand.
  void ReportCoverage();

  StreamDispatcher stream_;
  DiagnosticDispatcher diagnostic_;
  FileTable files_{stream_};
  PlusArgsSource plusargs_;
  CoverageLog coverage_;
  CurrentRuntimeGuard current_runtime_guard_{*this};
  std::unique_ptr<Design> design_;
  // Every time slot with something pending, earliest first -- LRM 4.4's first
  // division of the event set, by time, with each slot holding the second
  // division, by region. A slot exists exactly while something is pending in
  // it, so having any at all is the whole of "the simulation has work left".
  // The slot at `now_` is the one running; naming a later time creates that
  // slot, which is what lets a nonblocking assignment carrying a delay reach
  // the NBA region of a future slot (LRM 4.4.2.4, 10.4.2). Node-based because
  // a slot runs as a reference into here while its own execution creates the
  // later slots it schedules into.
  std::map<SimTime, TimeSlot> slots_;
  // Final processes wait here rather than in any slot: LRM 9.2.3 runs them
  // after the last one, when no slot is left to hold them.
  RegistrationList finals_;
  // The activations a region drain is working through, held apart from the
  // region they came out of.
  RegistrationList draining_;
  std::vector<std::shared_ptr<RuntimeProcess>> processes_;
  // Secondary index: pointers into `processes_` keyed by owning scope. Kept
  // in lockstep with `processes_` (register push, teardown erase). Consumers
  // that ask "which processes belong to this scope" (hierarchical `disable`,
  // `wait fork` descendant walk, scope teardown, `%m` attribution) reach it
  // here without scanning the full registry.
  std::unordered_map<Scope*, std::vector<RuntimeProcess*>> processes_by_scope_;
  RuntimeProcess* current_process_ = nullptr;
  SimTime now_ = 0;
  std::int8_t global_precision_power_ = kDefaultTimePrecisionPower;
  value::TimeFormat time_format_;
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
// `unit_instance` is the module, interface, or program instance the process is
// declared within, which holds the seeds LRM 18.14.1 starts a static process
// from. It is the scope itself unless the process is declared inside a generate
// scope, which has no seeds of its own.
void RegisterInitialProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine);
void RegisterFinalProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine);

}  // namespace lyra::runtime
