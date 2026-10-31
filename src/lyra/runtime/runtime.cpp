#include "lyra/runtime/runtime.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <ctime>
#include <format>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/program_threads.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/support/scope_execution.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

// What the run has cost the processor so far, which is the statistic LRM 20.2
// Table 20-1's most verbose level asks a tool to report.
auto ProcessorSeconds() -> double {
  return static_cast<double>(std::clock()) /
         static_cast<double>(CLOCKS_PER_SEC);
}

auto Times(std::size_t count) -> std::string {
  return count == 1 ? "once" : std::format("{} times", count);
}

}  // namespace

Runtime::Runtime() : Runtime(RuntimeOptions{}) {
}

Runtime::Runtime(RuntimeOptions options)
    : dumps_(options.plusargs),
      randomize_settings_(options.plusargs),
      plusargs_(std::move(options.plusargs)) {
  diagnostic_.SetContextSource([this] { return ReportContext(); });
}

Runtime::~Runtime() = default;

void Runtime::BindDesign(std::unique_ptr<Design> design) {
  if (bound_) {
    throw InternalError("Runtime::BindDesign called more than once");
  }
  bound_ = true;
  design_ = std::move(design);
  // The whole tree already exists: the generated `$root` constructor built
  // the top-level units as its owned children, and each child built its
  // own subtree. Resolving is one top-down walk from the root that recurses
  // through the owned-children relation, so the design-wide barrier holds --
  // every scope resolves before any initializes.
  WalkResolve(design_->Root());
}

auto Runtime::DesignRoot() -> Scope& {
  if (design_ == nullptr) {
    throw InternalError(
        "Runtime::DesignRoot: no design is bound -- please report this as a "
        "bug");
  }
  return design_->Root();
}

void Runtime::WalkResolve(Scope& scope) {
  scope.Resolve();
  scope.ForEachChild([this](Scope& child) { WalkResolve(child); });
}

void Runtime::WalkInitialize(Scope& scope) {
  scope.Initialize();
  scope.ForEachChild([this](Scope& child) { WalkInitialize(child); });
}

void Runtime::WalkActivate(Scope& scope) {
  scope.CreateProcesses();
  scope.ForEachChild([this](Scope& child) { WalkActivate(child); });
}

auto Runtime::Run() -> int {
  EnsureReadyToRun();
  ResolveGlobalTimePrecision();
  dumps_.Begin(*this, design_->Root(), global_precision_power_);
  RunSimulation();
  // The dump file is complete once the run is: what it recorded is written
  // and the file ended, a failure to write it reported as the design's own.
  RunAsLanding(*this, [this] { dumps_.End(); });
  // Owed because the run is over, whichever way it ended: LRM 16.3 asks a tool
  // with no assertion API to report immediate cover results at the end of
  // simulation.
  ReportCoverage();
  return diagnostic_.ReportedFatal() || tool_failed_ ? 1 : 0;
}

void Runtime::RunSimulation() {
  try {
    // LRM 4: every scope initializes before any activates, so a time-zero
    // initializer reads sealed endpoints and no process runs before all of
    // them have.
    //
    // Neither stretch runs in a process, so whatever leaves one lands here. An
    // initializer that ends the run -- by asking (LRM 20.2) or by an error of
    // the design (LRM 20.10) -- leaves the initialization itself, and what is
    // left of it does not run. The rest of the run still walks to its end --
    // creating a process runs none of its statements, and the final procedures
    // run because the simulation reached its end (LRM 9.2.3). Creating
    // processes is therefore left only by a failure of the tool, which is
    // reported and ends the run.
    RunAsLanding(*this, [this] { WalkInitialize(design_->Root()); });
    RunAsLanding(*this, [this] { WalkActivate(design_->Root()); });
    StartFrom(design_starting_, 0);
    StartFrom(program_starting_, 0);

    while (std::holds_alternative<Running>(state_)) {
      ExecuteTimeSlot();
      // LRM 9.2.3: a run reaches the end of simulation time by exhausting its
      // work as much as by being asked to end.
      if (std::holds_alternative<Running>(state_) && !AdvanceToNextSlot()) {
        state_ = SimulationEnded{};
      }
    }

    // LRM 9.2.3: a final procedure occurs at the end of simulation time. A tool
    // that cannot carry the run on has no such end to offer, and running the
    // design's procedures over a state already known to be wrong is not one.
    const bool ends_the_simulation = std::visit(
        Overloaded{
            [](const Running&) -> bool {
              throw InternalError(
                  "Runtime::RunSimulation: the region loop ended with the "
                  "simulation still running -- please report this as a bug");
            },
            [](const SimulationEnded&) { return true; },
            [](const ToolStopped&) { return false; }},
        state_);
    if (ends_the_simulation) {
      // An evaluation attempt still in flight is one no tick will settle now,
      // so it comes to what the finished trace says of it (Annex F.5.3.1) --
      // before the finals, which read what its statements wrote.
      for (EvaluationAttempts* attempts : concurrent_assertions_) {
        attempts->SettleAtEndOfRun();
      }
      ExecuteFinalProcedures();
      // What the design asked to have done once it is over -- writing the
      // coverage database (LRM 19.9) -- reads the same state the finals do.
      for (OwnedCall& effect : at_run_end_) {
        RunAsLanding(*this, effect);
      }
    }
  } catch (const std::exception&) {
    // A control effect is not derived from this hierarchy (LRM 9.6.2), so one
    // reaching here left its owner: that is a defect of the tool, and it
    // surfaces rather than being reported as something the design did.
    ReportRaisedError(*this, std::current_exception());
  }
}

void Runtime::RequestSimulationEnd() {
  ++end_requests_;
  // What is left of a region pass still runs after the tool stops the run, and
  // one of it asking to end does not make that the design's end, whose final
  // procedures would read a state already known to be wrong.
  if (!std::holds_alternative<ToolStopped>(state_)) {
    state_ = SimulationEnded{};
  }
}

void Runtime::RequestToolStop() {
  ++end_requests_;
  state_ = ToolStopped{};
}

void Runtime::ReportDesignError(const base::DiagMessage& message) {
  diagnostic_.Report(Severity::kFatal, message);
  RequestSimulationEnd();
}

void Runtime::ReportToolFailure(std::string_view message) {
  tool_failed_ = true;
  DiagnosticDispatcher::Line(std::format("error: {}", message));
  RequestToolStop();
}

void Runtime::ReportSimulationControl(
    std::string_view task, std::string_view origin, int finish_number) {
  // A finish number other than 0, 1 or 2 has no meaning in the standard, and
  // the front end accepts one (LRM 20.2). It prints what an omitted one prints.
  const int selected =
      finish_number < 0 || finish_number > 2 ? 1 : finish_number;
  if (selected == 0) {
    return;
  }
  std::string line;
  if (!origin.empty()) {
    line += origin;
    line += ": ";
  }
  line += std::format("note: '{}' at time {}", task, now_.load());
  if (selected == 2) {
    line += std::format(", {:.3f}s of processor time", ProcessorSeconds());
  }
  DiagnosticDispatcher::Line(line);
}

auto Runtime::ReportContext() const -> DiagnosticDispatcher::Context {
  const RuntimeProcess* const process = current_process_;
  const Scope* scope =
      process == nullptr ? nullptr : process->Context().procedure.scope;
  if (scope == nullptr) {
    return {.where = std::format("at time {}", now_.load()), .procedure = {}};
  }
  return {
      .where = std::format(
          "in '{}' at time {}",
          std::string_view{scope->HierarchicalPath().View()}, now_.load()),
      .procedure = process->Context().procedure.written_at};
}

void Runtime::ReportUnsettledSlot(std::span<const NotedProcedure> ran) {
  ReportDesignError(
      base::DiagMessage(
          "the design keeps scheduling work and time never advances"));

  // Each procedure once, in the order they first ran, with how many times it
  // ran.
  struct Counted {
    NotedProcedure procedure;
    std::size_t runs;
  };
  std::vector<Counted> counted;
  std::map<NotedProcedure, std::size_t> index_of;
  for (const NotedProcedure& procedure : ran) {
    const auto [at, first] = index_of.try_emplace(procedure, counted.size());
    if (first) {
      counted.push_back(Counted{.procedure = procedure, .runs = 0});
    }
    ++counted.at(at->second).runs;
  }
  // What keeps a slot going is what comes back most often, so that is named
  // first.
  std::ranges::stable_sort(counted, std::ranges::greater{}, &Counted::runs);

  const std::size_t named = std::min(counted.size(), kMaxNamedProcedures);
  for (const Counted& one : std::span{counted}.first(named)) {
    const std::string_view written_at = one.procedure.written_at;
    // A namespace's initializer runs in no scope, and an action it started
    // keeps that.
    const std::string where =
        one.procedure.scope == nullptr
            ? std::string{}
            : std::format(
                  " in '{}'",
                  std::string_view{
                      one.procedure.scope->HierarchicalPath().View()});
    DiagnosticDispatcher::Line(
        std::format(
            "{}{}note: this procedure{} ran {} in the last {} passes",
            written_at, written_at.empty() ? "" : ": ", where, Times(one.runs),
            kNotedRegionPasses));
  }
  if (named < counted.size()) {
    DiagnosticDispatcher::Line(
        std::format("note: and {} more procedures", counted.size() - named));
  }
}

void Runtime::ReportCoverage() {
  for (const std::string& line : coverage_.Report()) {
    std::cout << line << '\n';
  }
}

void Runtime::EnsureReadyToRun() {
  if (!bound_) {
    throw InternalError("Runtime::Run called before BindDesign");
  }
  if (ran_) {
    throw InternalError("Runtime::Run called more than once");
  }
  ran_ = true;
}

void Runtime::ResolveGlobalTimePrecision() {
  bool found = false;
  std::int8_t min_power = kDefaultTimePrecisionPower;
  design_->ForEachScope([&](Scope& scope) {
    const std::int8_t power = scope.TimePrecisionPower();
    if (power == abi::kUnspecifiedTimePower) {
      return;
    }
    min_power = found ? std::min(min_power, power) : power;
    found = true;
  });
  global_precision_power_ = found ? min_power : kDefaultTimePrecisionPower;
  // LRM Table 20-3: the default `%t` display unit is the design-global
  // precision (the smallest across all timescale directives).
  time_format_.units_power = global_precision_power_;
}

void Runtime::AdoptProcess(std::shared_ptr<RuntimeProcess> process) {
  roots_.Adopt(std::move(process));
}

void Runtime::RunAction(
    const ExecutionContext& context, Coroutine<void> coroutine, DrawRng rng) {
  if (current_process_ != nullptr) {
    throw InternalError(
        "Runtime::RunAction: an action starts from the region it is due in, "
        "between executions, and one is executing");
  }
  auto action = RuntimeProcess::Create(context, std::move(coroutine), rng);
  Activation* const top = action->TopActivation();
  roots_.Adopt(std::move(action));
  RuntimeProcess::Run(*this, top);
}

void Runtime::RegisterFinal(RegisteredFinal registered) {
  finals_.push_back(std::move(registered));
}

namespace {

// The turn a kind of process starts in at time zero.
auto TurnOf(StartKind kind) -> std::size_t {
  switch (kind) {
    case StartKind::kAlwaysProcedure:
      return 0;
    case StartKind::kContinuousDriver:
      return 1;
    case StartKind::kInitialProcedure:
      return 2;
    case StartKind::kTriggeredProcedure:
      return 3;
  }
  throw InternalError("TurnOf: a kind of starting process has no turn");
}

}  // namespace

void Runtime::QueueStart(StartKind kind, Activation* top) {
  // The region set is an ordered scale's two halves, so its first region says
  // which set it is.
  StartingTurns& starting = top->Process().Context().regions.active ==
                                    program_starting_.regions.active
                                ? program_starting_
                                : design_starting_;
  top->Queue(starting.turns.at(TurnOf(kind)));
}

void Runtime::StartFrom(StartingTurns& starting, std::size_t turn) {
  if (turn == starting.turns.size()) {
    return;
  }
  current_slot_[starting.regions.active].activations.Append(
      starting.turns.at(turn));
  // The Inactive region is reached once the Active one is empty (LRM 4.4.2.3),
  // which is when every process of this turn has run to its first wait, and
  // the same holds of their duals (LRM 4.4.2.7).
  Submit(now_, starting.regions.inactive, [this, &starting, turn] {
    StartFrom(starting, turn + 1);
  });
}

auto Runtime::ProgramOf(const Scope& program) -> ProgramThreads& {
  // Made with the program's first initial procedure, from which the program
  // keeps the run open (LRM 24.3); one with none is never made.
  return programs_.try_emplace(&program).first->second;
}

void Runtime::ProgramEnded() {
  // LRM 24.3: the implicit `$finish` has no call and no place, so it prints
  // nothing, and the run ends as one that runs out of work does. It ends a
  // simulation that is still running; once the simulation has ended there is
  // nothing left for it to end, so a final procedure that ends the last program
  // leaves the others to run (LRM 9.2.3).
  if (std::holds_alternative<Running>(state_) &&
      std::ranges::all_of(
          std::views::values(programs_), &ProgramThreads::AllRootsEnded)) {
    RequestSimulationEnd();
  }
}

auto Runtime::ClaimNamespaceInitialization(std::string_view name) -> bool {
  return namespace_seeds_.try_emplace(std::string{name}).second;
}

auto Runtime::NamespaceSeeds(std::string_view name) -> InitializationRng& {
  const auto found = namespace_seeds_.find(std::string{name});
  if (found == namespace_seeds_.end()) {
    throw InternalError(
        "Runtime::NamespaceSeeds: a namespace is initialized only by the call "
        "that claimed its bring-up");
  }
  return found->second;
}

void Runtime::EnterStaticInit(const ExecutionContext& context) {
  displacing_.emplace_back(context.SplitInitializationRng(), context, running_);
  running_ = &displacing_.back().state;
}

auto Runtime::RunningContext() const -> const ExecutionContext& {
  if (const RuntimeProcess* const process = current_process_) {
    return process->Context();
  }
  if (displacing_.empty()) {
    throw InternalError(
        "Runtime::RunningContext: generated code runs only inside a process "
        "or a static initialization, and neither is running");
  }
  return displacing_.back().context;
}

void Runtime::LeaveStaticInit() {
  running_ = displacing_.back().displaced;
  displacing_.pop_back();
}

auto Runtime::FutureSlotAt(SimTime when) -> TimeSlot& {
  if (when <= now_) {
    throw InternalError(
        "Runtime::FutureSlotAt: only a time after the current one has a future "
        "slot -- please report this as a bug");
  }
  return future_slots_.try_emplace(when, effect_storage_).first->second;
}

auto Runtime::AdvanceToNextSlot() -> bool {
  // Time moves only to a slot that holds something (LRM 4.5): one whose every
  // event was taken back -- the delay of a process stopped or killed before its
  // time came -- is passed over where it stands.
  while (!future_slots_.empty()) {
    auto next = future_slots_.extract(future_slots_.begin());
    if (!next.mapped().Empty()) {
      now_ = next.key();
      current_slot_.Append(next.mapped());
      return true;
    }
  }
  return false;
}

void Runtime::ExecuteTimeSlot() {
  RunRegion(Region::kPreponed);
  SlotPasses passes;
  // LRM 4.5: the active region set is run until it is empty, then the reactive
  // one, and the two again while either holds anything -- so what the reactive
  // set schedules into the active one waits until the reactive set is empty.
  while (current_slot_.FirstPending(Region::kActive, Region::kReNba)) {
    if (!DrainRegions(Region::kActive, Region::kObserved, passes) ||
        !DrainRegions(Region::kReactive, Region::kReNba, passes)) {
      return;
    }
  }
  RunRegion(Region::kPostponed);
  if (std::holds_alternative<Running>(state_) && !current_slot_.Empty()) {
    // LRM 4.4.2.9.
    ReportDesignError(
        base::DiagMessage(
            "the postponed region scheduled work back into the time slot that "
            "ends with it"));
  }
}

auto Runtime::DrainRegions(Region first, Region last, SlotPasses& passes)
    -> bool {
  // What a region produces lands back in the slot, so the earliest one holding
  // anything is looked for again after each.
  while (const std::optional<Region> region =
             current_slot_.FirstPending(first, last)) {
    if (++passes.count > kMaxRegionPassesPerSlot) {
      ReportUnsettledSlot(passes.ran);
      return false;
    }
    if (passes.count > kMaxRegionPassesPerSlot - kNotedRegionPasses) {
      // What a pass runs is what its region holds as the pass begins, and a
      // procedure is noted from there because its run may be the one that ends
      // it.
      current_slot_[*region].activations.ForEach(
          [&passes](const QueuePlace& queued) {
            const RuntimeProcess& process = queued.activation->Process();
            passes.ran.push_back(
                NotedProcedure{
                    .written_at = process.Context().procedure.written_at,
                    .scope = process.Context().procedure.scope});
          });
    }
    RunRegion(*region);
  }
  return true;
}

void Runtime::RunRegion(Region region) {
  RegionQueue& queue = current_slot_[region];
  // LRM 9.3.2: work arriving while this pass runs belongs to the next pass, so
  // what the region holds moves out of it and new arrivals accumulate behind.
  draining_.activations.Append(queue.activations);
  draining_.timers.Append(queue.timers);
  draining_.effects.Append(queue.effects);
  // LRM 4.6: nonblocking updates are performed in the order they were placed.
  // A deferred effect -- a nonblocking update, a report maturing, an
  // assertion's action -- runs with no activation holding it, so each runs as
  // a landing of its own: a run-time error it raises is reported and the
  // region goes on to the effects after it.
  draining_.effects.Drain([this](const auto& effect) {
    RunAsLanding(*this, effect);
    TellExpired();
  });
  // A timer comes off its queue as it fires, so arming it again from what the
  // firing wakes queues a new event rather than moving this one. What it does
  // runs with no activation holding it, as a deferred effect does, so it is a
  // landing of its own too.
  while (TimerPlace* fired = draining_.timers.PopFront()) {
    RunAsLanding(*this, [fired] { fired->timer->Fire(); });
    TellExpired();
  }
  while (QueuePlace* queued = draining_.activations.PopFront()) {
    Activation* activation = queued->activation;
    ConsumeWait(activation);
    RunProcess(activation);
    TellExpired();
  }
}

// What is told moves out of the list first, so an object expiring while the
// waits are told -- by what a wake runs at the occurrence -- is told in the
// next round, and a wait ending meanwhile takes its membership off whichever
// list holds it.
void Runtime::TellExpired() {
  while (!expired_.Empty()) {
    IntrusiveList<WaitMembership> telling;
    telling.Append(expired_);
    WakeParkedOn(telling, Change::Whole());
  }
}

namespace {

// One final procedure's own facts in force on the thread calling it, for as
// long as the call lasts, whichever way it is left.
class InFinalProcedure {
 public:
  InFinalProcedure(RuntimeProcess& thread, const ProcedureContext& procedure)
      : thread_(&thread), left_(thread.EnterProcedure(procedure)) {
  }
  ~InFinalProcedure() {
    thread_->LeaveProcedure(left_);
  }

  InFinalProcedure(const InFinalProcedure&) = delete;
  auto operator=(const InFinalProcedure&) -> InFinalProcedure& = delete;
  InFinalProcedure(InFinalProcedure&&) = delete;
  auto operator=(InFinalProcedure&&) -> InFinalProcedure& = delete;

 private:
  RuntimeProcess* thread_;
  ProcedureContext left_;
};

}  // namespace

void Runtime::ExecuteFinalProcedures() {
  // The thread exists only for the calls, so it is made once the simulation
  // has ended. It is no static process (LRM Annex P) and no thread created it,
  // so LRM 18.14.1 says nothing of its generator; it is split from the root's
  // initialization RNG, last of all, which moves no draw made before.
  Scope& root = design_->Root();
  auto thread = RuntimeProcess::Create(
      ExecutionContext{
          .procedure = {.seeds = &root.InitializationSeeds()},
          .creator = ThreadCreator::kAnotherConstruct},
      CallingFinalProcedures(), root.InitializationSeeds().Split());
  Activation* const top = thread->TopActivation();
  AdoptProcess(std::move(thread));
  RuntimeProcess::Run(*this, top);
}

auto Runtime::CallingFinalProcedures() -> Coroutine<void> {
  RuntimeProcess& thread = CurrentProcess();
  const std::uint64_t requests_before = end_requests_;
  for (RegisteredFinal& registered : finals_) {
    {
      // Each call is a landing: whatever leaves the procedure -- a `$finish`,
      // or a run-time error, reported in the procedure's own scope -- stops
      // there.
      const InFinalProcedure in_it(thread, registered.procedure);
      RunAsLanding(*this, registered.call);
    }
    // LRM 9.2.3: a `$finish` reached inside a final procedure ends the
    // simulation immediately, so the ones still registered do not run. The
    // request is what says so, however the procedure itself came to an end.
    if (end_requests_ != requests_before) {
      break;
    }
  }
  co_return;
}

void Runtime::RunProcess(Activation* activation) {
  // Where an ending stops the design: no process resumes after one. Deferred
  // effects the slot already holds still run, and the final procedures are
  // called through their own path.
  if (!std::holds_alternative<Running>(state_)) {
    return;
  }
  // No wait dispatch: an execution that stopped to wait arranged its own way
  // back before it gave up control, and one that ended already woke what
  // awaits it and its parent's `wait fork` (LRM 9.3.2, 9.7, 9.6.1).
  RuntimeProcess::Run(*this, activation);
}

namespace {

// Where the design's code runs and where a program's does (LRM 24.3.1).
auto RegionsOf(support::ScopeExecution execution) -> RegionSet {
  switch (execution) {
    case support::ScopeExecution::kDesign:
      return kActiveRegionSet;
    case support::ScopeExecution::kProgram:
      return kReactiveRegionSet;
  }
  throw InternalError("RegionsOf: an instance run no way");
}

// What a procedure belongs to: the scope its reports name and the seeds of the
// instance whose text holds it (LRM 18.14.1).
auto ProcedureIn(
    Scope* owning_scope, Scope* unit_instance, const char* written_at)
    -> ProcedureContext {
  return ProcedureContext{
      .scope = owning_scope,
      .seeds = &unit_instance->InitializationSeeds(),
      .written_at = written_at};
}

// What an execution runs in: the procedure it runs, the region set the class
// of the instance whose code it evaluates states (LRM 24.3.1), and what
// created its thread. It belongs to no program; an initial procedure of one is
// made a member where it registers.
auto ContextIn(
    Scope* owning_scope, Scope* unit_instance, Scope* runs_as,
    const char* written_at, ThreadCreator creator) -> ExecutionContext {
  return ExecutionContext{
      .procedure = ProcedureIn(owning_scope, unit_instance, written_at),
      .regions = RegionsOf(runs_as->Info().metadata.Execution()),
      .creator = creator};
}

// A continuous assignment's driver is a thread of its own (LRM 10.3) that no
// procedure created; every other kind that starts at time zero is an initial or
// always procedure (LRM 9.2).
auto CreatorOf(StartKind kind) -> ThreadCreator {
  switch (kind) {
    case StartKind::kAlwaysProcedure:
    case StartKind::kInitialProcedure:
    case StartKind::kTriggeredProcedure:
      return ThreadCreator::kInitialOrAlwaysProcedure;
    case StartKind::kContinuousDriver:
      return ThreadCreator::kAnotherConstruct;
  }
  throw InternalError("CreatorOf: a kind of starting process has no creator");
}

auto StartingProcess(const ExecutionContext& context, Coroutine<void> coroutine)
    -> std::shared_ptr<RuntimeProcess> {
  return RuntimeProcess::Create(
      context, std::move(coroutine), context.SplitInitializationRng());
}

void RegisterStartingProcess(
    StartKind kind, Scope* owning_scope, Scope* unit_instance, Scope* runs_as,
    Coroutine<void> coroutine, const char* written_at) {
  Runtime& rt = AsRuntime(current_runtime());
  auto process = StartingProcess(
      ContextIn(
          owning_scope, unit_instance, runs_as, written_at, CreatorOf(kind)),
      std::move(coroutine));
  rt.QueueStart(kind, process->TopActivation());
  rt.AdoptProcess(std::move(process));
}

}  // namespace

void RegisterAlwaysProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at) {
  RegisterStartingProcess(
      StartKind::kAlwaysProcedure, owning_scope, unit_instance, unit_instance,
      std::move(coroutine), written_at);
}

void RegisterContinuousDriver(
    Scope* owning_scope, Scope* unit_instance, Scope* runs_as,
    Coroutine<void> coroutine, const char* written_at) {
  RegisterStartingProcess(
      StartKind::kContinuousDriver, owning_scope, unit_instance, runs_as,
      std::move(coroutine), written_at);
}

// A program's initial procedure is one of its roots, which the program keeps,
// and every thread it starts is one of the program's (LRM 24.3, 24.7).
void RegisterInitialProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at) {
  switch (unit_instance->Info().metadata.Execution()) {
    case support::ScopeExecution::kDesign:
      RegisterStartingProcess(
          StartKind::kInitialProcedure, owning_scope, unit_instance,
          unit_instance, std::move(coroutine), written_at);
      return;
    case support::ScopeExecution::kProgram: {
      Runtime& rt = AsRuntime(current_runtime());
      ProgramThreads& program = rt.ProgramOf(*unit_instance);
      auto process = StartingProcess(
          ContextIn(
              owning_scope, unit_instance, unit_instance, written_at,
              CreatorOf(StartKind::kInitialProcedure))
              .InProgram(program),
          std::move(coroutine));
      rt.QueueStart(StartKind::kInitialProcedure, process->TopActivation());
      program.AdoptRoot(std::move(process));
      return;
    }
  }
  throw InternalError("RegisterInitialProcess: an instance run no way");
}

void Exit(RuntimeEffects& runtime, const value::String& origin) {
  ProgramThreads* const program = AsRuntime(runtime).RunningContext().program;
  if (program == nullptr) {
    runtime.Diagnostic().EmitWarning(
        origin, value::String(
                    "$exit ignored, since this thread does not originate in "
                    "an initial procedure of a program (LRM 24.7)"));
    return;
  }
  program->Exit(runtime);
}

void RegisterTriggeredProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at) {
  RegisterStartingProcess(
      StartKind::kTriggeredProcedure, owning_scope, unit_instance,
      unit_instance, std::move(coroutine), written_at);
}

void RegisterFinalProcedure(
    Scope* owning_scope, Scope* unit_instance,
    OwnedClosure<support::EffectSignature> call, const char* written_at) {
  AsRuntime(current_runtime())
      .RegisterFinal(
          RegisteredFinal{
              .procedure = ProcedureIn(owning_scope, unit_instance, written_at),
              .call = std::move(call)});
}

void RunAssertionAction(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at) {
  const ExecutionContext context = ContextIn(
      owning_scope, unit_instance, unit_instance, written_at,
      ThreadCreator::kAnotherConstruct);
  AsRuntime(current_runtime())
      .RunAction(
          context, std::move(coroutine), context.SplitInitializationRng());
}

void EnterScopeStaticInit(RuntimeEffects& runtime, Scope* unit_instance) {
  AsRuntime(runtime).EnterStaticInit(
      ExecutionContext{
          .procedure = ProcedureIn(unit_instance, unit_instance, ""),
          .creator = ThreadCreator::kAnotherConstruct});
}

void EnterNamespaceStaticInit(RuntimeEffects& runtime, const char* name) {
  Runtime& rt = AsRuntime(runtime);
  rt.EnterStaticInit(
      ExecutionContext{
          .procedure = {.seeds = &rt.NamespaceSeeds(name)},
          .creator = ThreadCreator::kAnotherConstruct});
}

auto ClaimNamespaceInitialization(RuntimeEffects& runtime, const char* name)
    -> std::int64_t {
  return AsRuntime(runtime).ClaimNamespaceInitialization(name) ? 1 : 0;
}

void LeaveStaticInit(RuntimeEffects& runtime) {
  AsRuntime(runtime).LeaveStaticInit();
}

}  // namespace lyra::runtime
