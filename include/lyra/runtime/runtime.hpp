#pragma once

#include <array>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <map>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/bound_members.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/coverage.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/mem_file.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/owned_processes.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/program_threads.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/time_slot.hpp"
#include "lyra/runtime/value_dumps.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class Design;
class Observable;
class RuntimeProcess;
class Scope;

struct RuntimeOptions {
  // LRM 21.6 command-line plusarg tokens with the `+` prefix already
  // stripped, in the order they appeared on the host command line.
  std::vector<std::string> plusargs;
};

// The kinds of process that start at time zero. They start in turn, in the
// order written here, each turn once the one before it has run to its first
// waits. The standard leaves the order of one slot's processes open (LRM 4.7),
// and this is the one a design can lean on either way:
//
// - An `always` procedure is waiting before a continuous driver first
//   evaluates, so the first value a driver gives a name is a change the
//   procedure sees (LRM 4.9.1). A register whose only reset is tied to a
//   constant is reset by that change.
// - An `initial` procedure starts once the drivers have evaluated, so it reads
//   what a constant drives and not the value the name held before.
// - An `always_comb` or `always_latch` is triggered after all initial and
//   always procedures have been started (LRM 9.2.2.2).
enum class StartKind : std::uint8_t {
  kAlwaysProcedure,
  kContinuousDriver,
  kInitialProcedure,
  kTriggeredProcedure,
};

inline constexpr std::size_t kStartTurnCount =
    static_cast<std::size_t>(StartKind::kTriggeredProcedure) + 1;

// The concrete simulation runtime. Owns every mutable piece of simulator
// state -- time, region queues, execution ambient, the file table, the attached
// design, the executions no process started -- and drives the resolve walk and
// the run.
// Generated code sees only the `RuntimeEffects` view; the host boundary here
// (BindDesign, Run) is not visible through that view.
class Runtime final : public RuntimeEffects {
 public:
  Runtime();
  explicit Runtime(RuntimeOptions options);

  Runtime(const Runtime&) = delete;
  auto operator=(const Runtime&) -> Runtime& = delete;
  Runtime(Runtime&&) = delete;
  auto operator=(Runtime&&) -> Runtime& = delete;
  ~Runtime();

  // Takes ownership of the elaborated `design` and resolves every scope's
  // cross-instance references, which is the last of elaboration (LRM 3.12).
  // Initializing state and creating processes are simulation activity at time
  // zero, so they belong to the run rather than to binding.
  void BindDesign(std::unique_ptr<Design> design);
  // The `$root` of the bound design. What a foreign call resolves a scope name
  // against is the design, so the boundary that stands the run up reads it from
  // here rather than from whoever happened to allocate the tree.
  [[nodiscard]] auto DesignRoot() -> Scope&;
  auto Run() -> int;

  // The simulation reached its end -- the design asked (LRM 20.2), or a
  // run-time error of its own ended it the way `$fatal` does (LRM 20.10) -- so
  // the run walks the whole of what it owes.
  void RequestSimulationEnd();
  // The tool cannot carry the run on, so the design's end-of-simulation
  // procedures do not run: the state they would read is already known to be
  // wrong.
  void RequestToolStop();

  // A run-time error of the design: the fatal report LRM 20.10 asks a tool to
  // make for it, and the implicit `$finish` that follows one.
  void ReportDesignError(const base::DiagMessage& message);

  // A failure of the tool itself, which no SystemVerilog construct can express
  // and no severity describes, so it is stated plainly and ends the run
  // without the design's own ending.
  void ReportToolFailure(std::string_view message);

  // LRM 20.2, Table 20-1: what a simulation control task prints, chosen by its
  // finish number, where `task` names the one that was called and `origin` is
  // its source location.
  void ReportSimulationControl(
      std::string_view task, std::string_view origin, int finish_number);

  // Keeps `process`, which no process started: a procedure, or the carrier of
  // an update. It is kept for as long as it or anything it started runs.
  // Public because the free registration functions live outside this class.
  void AdoptProcess(std::shared_ptr<RuntimeProcess> process);

  // Runs an assertion's pass or fail statement as an execution of no lineage
  // (LRM 16.4, 16.14.1), here and to its first stop, and keeps it until it
  // ends. It runs at once rather than being queued: its caller is the region
  // the statement is due in, where nothing runs ahead of it, or the end of the
  // run, where nothing queued would run at all. Public because the entries
  // that start an assertion's actions live outside this class.
  void RunAction(
      const ExecutionContext& context, Coroutine<void> coroutine, DrawRng rng);

  // LRM 9.2.3: a `final` procedure waits for the end of simulation, after the
  // last slot, rather than in any slot.
  void QueueFinal(Activation* top);

  // A process that starts at time zero waits for the turn its kind has, in the
  // region set it runs in: the design's, or a program's (LRM 24.3.1).
  void QueueStart(StartKind kind, Activation* top);

  // The threads of the program instance `program`.
  [[nodiscard]] auto ProgramOf(const Scope& program) -> ProgramThreads&;

  // A program's initial procedures have all ended. Once that holds of every
  // program the run ends, by the implicit `$finish` LRM 24.3 makes.
  void ProgramEnded();

  // Takes a static initialization on as what is running, and gives it back.
  // It runs in `context`, whose initialization RNG hands it its generator (LRM
  // 18.14.1). Public because the entries generated code reaches live outside
  // this class.
  void EnterStaticInit(const ExecutionContext& context);
  void LeaveStaticInit();

  // What the running process or static initialization runs in.
  [[nodiscard]] auto RunningContext() const -> const ExecutionContext&;

  // The initialization RNG of the namespace named `name`, whose bring-up has
  // been claimed (LRM 18.14.1). Held for the run, as an instance's is.
  [[nodiscard]] auto NamespaceSeeds(std::string_view name)
      -> InitializationRng&;

  // Takes the one bring-up a namespace's initializers get, answering false
  // where something already took it. A namespace's initializers run before any
  // procedure starts (LRM 26.2) and each is reached both by the design's own
  // bring-up and by every namespace whose initializers read it, so the entry
  // is asked more than once and must run once. Held for the run rather than in
  // the generated code, so a second design brought up in one process starts
  // with none of them taken.
  auto ClaimNamespaceInitialization(std::string_view name) -> bool;

 private:
  friend class RuntimeEffects;
  friend class CurrentRuntimeGuard;
  friend class ProcessExecutionGuard;
  friend class InterruptReport;

  // The run has not been asked to end. Leaving the region loop in this state
  // means it ran out of work, which is an end of simulation like any other.
  struct Running {};
  struct SimulationEnded {};
  struct ToolStopped {};
  using RunState = std::variant<Running, SimulationEnded, ToolStopped>;

  // How many times one slot may re-enter its regions before the design is
  // declared unable to settle. A design that keeps scheduling work at the
  // current time never advances, so this bound is what turns a run that would
  // not end into one that reports why.
  static constexpr std::size_t kMaxRegionPassesPerSlot = 10000;
  // How many of a slot's last passes before that bound say which procedures
  // they ran. A slot that settles never reaches them, so only a run about to be
  // stopped pays for what its report needs.
  static constexpr std::size_t kNotedRegionPasses = 10;
  // How many procedures the report of an unsettled slot names one by one.
  static constexpr std::size_t kMaxNamedProcedures = 8;

  // One procedure an unsettled slot was still running: where it is written and
  // the scope it runs in.
  struct NotedProcedure {
    const char* written_at;
    const Scope* scope;

    auto operator<=>(const NotedProcedure&) const = default;
  };

  // How many times a slot has entered its regions, and what its last passes
  // ran, which the bound on its passes reports.
  struct SlotPasses {
    std::size_t count = 0;
    std::vector<NotedProcedure> ran;
  };

  // The slot at `when`. Asked on every wake and every nonblocking update, so
  // the current slot is answered inline and only a later one costs a call.
  auto SlotAt(SimTime when) -> TimeSlot& {
    return when == now_ ? current_slot_ : FutureSlotAt(when);
  }
  // A later slot, created on first use. The current slot is held apart, and an
  // earlier one would never run because the simulator never goes backwards in
  // time (LRM 4.4), so naming either faults here rather than placing work where
  // it would run out of order or be lost.
  auto FutureSlotAt(SimTime when) -> TimeSlot&;
  // LRM 4.4: moves time to the earliest later slot that holds something and
  // makes its events the current slot's, or answers false where none is left.
  auto AdvanceToNextSlot() -> bool;

  void EnsureReadyToRun();
  // LRM 3.14.3: design-global tick is the minimum declared precision across
  // the tree.
  void ResolveGlobalTimePrecision();
  void WalkResolve(Scope& scope);
  void WalkInitialize(Scope& scope);
  void WalkActivate(Scope& scope);
  // The processes that start at time zero in one region set, by turn, until
  // each turn comes.
  struct StartingTurns {
    RegionSet regions;
    std::array<IntrusiveList<QueuePlace>, kStartTurnCount> turns;
  };

  // Releases the processes of `turn` into the first slot, and each later turn
  // once the one before it has run to its first waits.
  void StartFrom(StartingTurns& starting, std::size_t turn);

  void RunProcess(Activation* activation);

  // LRM 4.5: Preponed; then the active region set run until it is empty and
  // the reactive set likewise, repeated until nothing in Active through Re-NBA
  // remains; then Postponed.
  void ExecuteTimeSlot();
  // Runs the earliest of the regions `first` through `last` holding anything
  // until none does, answering false where the slot met the bound on its passes
  // and was reported.
  auto DrainRegions(Region first, Region last, SlotPasses& passes) -> bool;
  void RunRegion(Region region);
  // The fatal report of a slot that met the bound on its passes, and what its
  // last passes ran, a procedure once for each time it ran.
  void ReportUnsettledSlot(std::span<const NotedProcedure> ran);
  void ExecuteFinalProcesses();
  // LRM 4: variable initialization and process activation are simulation
  // activity at time zero, so a run-time error raised by either is the design's
  // and ends the run the same way one raised later does.
  void RunSimulation();
  // LRM 20.10: where in the design and when a report is being made.
  [[nodiscard]] auto ReportContext() const -> DiagnosticDispatcher::Context;
  // LRM 16.3 requires a tool to report immediate cover results at the end of
  // simulation where it offers no assertion API to ask for them on demand.
  void ReportCoverage();

  DiagnosticDispatcher diagnostic_;
  FileTable files_;
  // Built from the plusargs before they are moved into their own source.
  ValueDumps dumps_;
  PlusArgsSource plusargs_;
  CoverageLog coverage_;
  CurrentRuntimeGuard current_runtime_guard_{*this};
  std::unique_ptr<Design> design_;
  // LRM 4.4 divides the event set first by time, into the current slot and
  // later ones, and each slot by region. Nearly every event -- a wake, a
  // nonblocking update with no delay -- is placed in the current slot, so it is
  // held for the whole run and its queues keep their storage from one slot to
  // the next.
  TimeSlot current_slot_;
  // Every later slot something was placed in, earliest first. Naming a later
  // time creates its slot, which is what lets a nonblocking assignment carrying
  // a delay reach the NBA region of a future slot (LRM 4.4.2.4, 10.4.2). One
  // whose every event was taken back stays until time passes it over.
  std::map<SimTime, TimeSlot> future_slots_;
  // Final processes wait here rather than in any slot: LRM 9.2.3 runs them
  // after the last one, when no slot is left to hold them.
  IntrusiveList<QueuePlace> finals_;
  // What is owed because the simulation is over, held from wherever the run
  // asked for it and run once the final procedures have.
  std::vector<OwnedCall> at_run_end_;
  // What an occurrence woke to run at it, held while the waits it reached are
  // still being asked, and how deep that asking is.
  std::vector<Activation*> woken_at_the_occurrence_;
  int occurrences_being_told_ = 0;
  // The design's processes start in the active region set, and a program's in
  // the reactive one once the design's have settled (LRM 24.3.1).
  StartingTurns design_starting_{.regions = kActiveRegionSet, .turns = {}};
  StartingTurns program_starting_{.regions = kReactiveRegionSet, .turns = {}};
  // Every concurrent assertion the design activated, so an attempt no tick
  // settled can be answered before the finals run.
  std::vector<EvaluationAttempts*> concurrent_assertions_;
  // What a region pass is working through, held apart from the region it came
  // out of, which meanwhile collects the next pass's work (LRM 9.3.2). Held for
  // the whole run so its storage outlives each pass.
  RegionQueue draining_;
  BoundMembers bound_members_;
  std::unordered_set<const VariableCell*> driven_continuously_;
  // Every execution no process started and no program keeps. Released before
  // the queues above, which a running one is still placed in.
  OwnedProcesses roots_{nullptr};
  // Each program instance's threads, by the instance, released before the
  // queues above as the runtime's own are. A map holds each where it was made,
  // which its roots point back at.
  std::map<const Scope*, ProgramThreads> programs_;
  // The process running now. Atomic, as the current time is, because a run
  // stopped from outside is asked where it is from a signal handler, which may
  // read nothing else.
  std::atomic<RuntimeProcess*> current_process_ = nullptr;
  // What is running, which is what a randomization call draws from (LRM 18.13,
  // 18.14) and what a DPI-C foreign call reports its scope through (LRM
  // 35.5.3). Whichever is running installs one: a process the state it owns,
  // a static initialization one of its own.
  RunningState* running_ = nullptr;
  // The stream of every `$random` called with no seed (LRM 20.14.1). The
  // standard states no start for it; zero is what the algorithm itself
  // replaces with its own constant (LRM Annex N).
  std::int32_t unseeded_random_seed_ = 0;
  // The static initializations under way, which own no object to keep their
  // state on, and whose entry and exit are two separate calls from generated
  // code with no frame spanning them -- so the runtime holds the stack. A deque
  // because the pointer above stays valid across a push.
  std::deque<DisplacingState> displacing_;
  // The initialization RNG of each namespace, by its name (LRM 18.14.1), held
  // for the run as an instance holds its own. A namespace is here once its one
  // bring-up has been taken: it reaches its initializers through whichever of
  // its readers got there first, so what orders them is this map plus the calls
  // each one makes, and no party holds the whole graph.
  std::unordered_map<std::string, InitializationRng> namespace_seeds_;
  std::atomic<SimTime> now_ = 0;
  std::int8_t global_precision_power_ = kDefaultTimePrecisionPower;
  value::TimeFormat time_format_;
  RunState state_ = Running{};
  // Every request that the run end, counted, because LRM 9.2.3 has a `$finish`
  // from within a `final` procedure end the simulation immediately and the tail
  // that would obey it is already running under an earlier request.
  std::uint64_t end_requests_ = 0;
  bool bound_ = false;
  bool ran_ = false;
  bool tool_failed_ = false;
};

// The runtime a `RuntimeEffects` view is a view of. `Runtime` is the only class
// deriving from the view, so every view is one.
[[nodiscard]] auto AsRuntime(RuntimeEffects& effects) -> Runtime&;
[[nodiscard]] auto AsRuntime(const RuntimeEffects& effects) -> const Runtime&;

// Reached by the generated registration builtins: creates a process bound to
// `owning_scope` and registers it in the ambient runtime. LRM 9.2 lifecycle:
// every kind but a `final` starts at time zero, in the turn its kind has; a
// `final` waits on the finals list until shutdown. The scope handle arrives as
// a pointer because the generated call site is holding the `self` pointer from
// its enclosing body. `unit_instance` is the module, interface, or program
// instance whose text creates the process, which holds the seeds LRM 18.14.1
// starts a static process from: the scope itself unless the process is
// declared inside a generate scope, which has no seeds of its own. A process
// runs as the code of that instance and in the region set its class states
// (LRM 24.3.1), except a continuous driver, which names the instance it runs
// as: the drive across a child's output port is created by the instantiation
// and evaluates the child's code (LRM 4.9.6, 24.3.2). `written_at` is where the
// source writes the procedure, as text that stands for the whole run.
void RegisterAlwaysProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at);
void RegisterContinuousDriver(
    Scope* owning_scope, Scope* unit_instance, Scope* runs_as,
    Coroutine<void> coroutine, const char* written_at);
void RegisterInitialProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at);
void RegisterTriggeredProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at);
void RegisterFinalProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at);

// LRM 24.7 `$exit`: ends the program the running thread originates in, the
// running thread with it; from any other thread it is ignored, and says so at
// `origin`, where the call is written.
void Exit(RuntimeEffects& runtime, const lyra::value::String& origin);

// Reached by the generated code running a concurrent assertion's pass or fail
// statement in the Reactive region, or at the end of the run for an attempt
// still pending (LRM 16.14.1, 16.12.2), with what a registered process is
// started from. No process starts the action, so `wait fork` and
// `disable fork` do not reach it (LRM 9.6.1, 9.6.3).
void RunAssertionAction(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at);

// A static initializer runs before any procedure starts (LRM 10.5, 26.2), so
// the generator it draws from cannot be a process's. LRM 18.14.1 names the one
// it is instead: the initialization RNG of the module, interface, or program
// instance -- or of the package -- the declaration sits in, which the standard
// requires to serve static initializers as well as static processes. These
// bracket such a run: the enclosing generated body enters one, initializes, and
// leaves on every way out.
//
// `unit_instance` is the scope holding those seeds: the scope itself unless the
// declaration sits in a generate scope, which keeps no seeds of its own. A
// namespace has no instance at all (LRM 26.2); its entry names the namespace,
// whose initialization RNG starts from the default seed.
void EnterScopeStaticInit(RuntimeEffects& runtime, Scope* unit_instance);
void EnterNamespaceStaticInit(RuntimeEffects& runtime, const char* name);
void LeaveStaticInit(RuntimeEffects& runtime);

// Whether this call is the one that brings up the namespace named `name`: 1 on
// the call that takes it, which happens exactly once per run, and 0 on every
// other. Its initializers must have run before any procedure starts (LRM 26.2),
// and a namespace whose own initializers read another's cells reaches that one
// first, so the entry is called both by the design's bring-up and by each such
// reader; taking the claim before descending is what ends a cycle among them.
//
// A machine word rather than a host `bool` because the generated-code ABI
// spends that return type on one question -- whether a call parks its caller --
// and this parks nobody.
auto ClaimNamespaceInitialization(RuntimeEffects& runtime, const char* name)
    -> std::int64_t;

}  // namespace lyra::runtime
