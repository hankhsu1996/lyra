#include "lyra/runtime/runtime.hpp"

#include <algorithm>
#include <array>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <ctime>
#include <format>
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
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/program_threads.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/support/file_descriptor.hpp"
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

// A procedure no process is running in force for as long as what it asked for
// runs, however that is left. What runs reports for that procedure.
class RunningAnEffect {
 public:
  RunningAnEffect(
      std::atomic<const ProcedureContext*>& in_force,
      const ProcedureContext& made_by)
      : in_force_(&in_force), outer_(in_force.load(std::memory_order_relaxed)) {
    in_force.store(&made_by, std::memory_order_relaxed);
  }
  ~RunningAnEffect() {
    in_force_->store(outer_, std::memory_order_relaxed);
  }

  RunningAnEffect(const RunningAnEffect&) = delete;
  auto operator=(const RunningAnEffect&) -> RunningAnEffect& = delete;
  RunningAnEffect(RunningAnEffect&&) = delete;
  auto operator=(RunningAnEffect&&) -> RunningAnEffect& = delete;

 private:
  std::atomic<const ProcedureContext*>* in_force_;
  const ProcedureContext* outer_;
};

}  // namespace

// Every effect runs inside one, and nearly none enters anything, so what it
// keeps is what was running as it began: whatever else installs what is running
// has given it back by the time the effect returns, so anything still standing
// then entered to run on no thread, and is left, innermost first.
class Runtime::NoThreadRunsScope {
 public:
  explicit NoThreadRunsScope(Runtime& runtime)
      : runtime_(&runtime), began_with_(runtime.running_) {
  }
  NoThreadRunsScope(const NoThreadRunsScope&) = delete;
  auto operator=(const NoThreadRunsScope&) -> NoThreadRunsScope& = delete;
  NoThreadRunsScope(NoThreadRunsScope&&) = delete;
  auto operator=(NoThreadRunsScope&&) -> NoThreadRunsScope& = delete;
  ~NoThreadRunsScope() {
    while (runtime_->running_ != began_with_) {
      runtime_->LeaveNoThread();
    }
  }

 private:
  Runtime* runtime_;
  const RunningState* began_with_;
};

Runtime::Runtime() : Runtime(RuntimeOptions{}) {
}

Runtime::Runtime(RuntimeOptions options)
    : command_line_(std::move(options.command_line)),
      dumps_(command_line_.Plusargs()),
      randomize_settings_(command_line_.Plusargs()) {
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
  // What the run wrote is delivered as it ends, and output lost while the
  // design was never told of it fails the run, as an output stream reports an
  // error its client never checked. A design that asks `$ferror` and is
  // answered handles the loss itself.
  const std::vector<LostOutput> lost = files_.Finish();
  for (const LostOutput& one : lost) {
    const std::array notes{base::DiagMessage(
        "ask '$ferror' after writing to handle it in the design")};
    diagnostic_.Report(
        base::kNoSourceLocation, Severity::kError,
        base::DiagMessage(
            "output written to {} was lost ({})", one.destination,
            one.refused.message()),
        notes);
  }
  return diagnostic_.ReportedFatal() || tool_failed_ || !lost.empty() ? 1 : 0;
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
    phase_ = Phase::kRunningRegions;

    while (std::holds_alternative<Running>(state_)) {
      ExecuteTimeSlot();
      // LRM 9.2.3: a run reaches the end of simulation time by exhausting its
      // work as much as by being asked to end.
      if (std::holds_alternative<Running>(state_) && !AdvanceToNextSlot()) {
        state_ = SimulationEnded{};
      }
    }

    phase_ = Phase::kOver;

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
      for (OwedAtRunEnd& owed : at_run_end_) {
        const RunningAnEffect running(procedure_in_force_, owed.procedure);
        RunAsLanding(*this, owed.call);
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

void Runtime::ReportDesignError(
    base::SourceLocation at, const base::DiagMessage& message) {
  diagnostic_.ReportAt(at, Severity::kFatal, message);
  RequestSimulationEnd();
}

void Runtime::ReportToolFailure(std::string_view message) {
  tool_failed_ = true;
  diagnostic_.Line(std::format("error: {}", message));
  RequestToolStop();
}

void Runtime::ReportSimulationControl(
    std::string_view task, int finish_number) {
  // A finish number other than 0, 1 or 2 has no meaning in the standard, and
  // the front end accepts one (LRM 20.2). It prints what an omitted one prints.
  const int selected =
      finish_number < 0 || finish_number > 2 ? 1 : finish_number;
  if (selected == 0) {
    return;
  }
  std::string line = std::format(
      "{}: note: '{}' at time {}",
      SourceLocationText(base::BoundSourceLocation(task)), task, now_.load());
  if (selected == 2) {
    line += std::format(", {:.3f}s of processor time", ProcessorSeconds());
  }
  diagnostic_.Line(line);
}

auto Runtime::ReportContext() const -> DiagnosticDispatcher::Context {
  const ProcedureContext& procedure = RunningProcedure();
  return {
      .scope = procedure.scope == nullptr
                   ? std::nullopt
                   : std::optional<std::string>{std::string{
                         procedure.scope->HierarchicalPath().View()}},
      .time = now_.load(),
      .written_at = procedure.written_at};
}

void Runtime::ReportUnsettledSlot(std::span<const ProcedureContext> ran) {
  ReportDesignFailure(
      base::kNoSourceLocation,
      base::DiagMessage(
          "the design keeps scheduling work and time never advances"));

  // Each procedure once, in the order they first ran, with how many times it
  // ran.
  struct Counted {
    ProcedureContext procedure;
    std::size_t runs;
  };
  std::vector<Counted> counted;
  std::map<ProcedureContext, std::size_t> index_of;
  for (const ProcedureContext& procedure : ran) {
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
    const std::string written_at = SourceLocationText(one.procedure.written_at);
    // A namespace's initializer runs in no scope, and an action it started
    // keeps that.
    const std::string where =
        one.procedure.scope == nullptr
            ? std::string{}
            : std::format(
                  " in '{}'",
                  std::string_view{
                      one.procedure.scope->HierarchicalPath().View()});
    diagnostic_.Line(
        std::format(
            "{}{}note: this procedure{} ran {} in the last {} passes",
            written_at, written_at.empty() ? "" : ": ", where, Times(one.runs),
            kNotedRegionPasses));
  }
  if (named < counted.size()) {
    diagnostic_.Line(
        std::format("note: and {} more procedures", counted.size() - named));
  }
}

void Runtime::ReportCoverage() {
  for (const std::string& line : coverage_.Report()) {
    files_.Writeln(support::kStdoutFd, value::String{line});
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
  InstallNoThreadRun(
      NoThreadContext{
          .context = context, .generator = context.SplitInitializationRng()},
      context.seeds);
}

void Runtime::EnterNoThread(const NoThreadContext& taken) {
  InstallNoThreadRun(taken, nullptr);
}

void Runtime::InstallNoThreadRun(
    const NoThreadContext& taken, InitializationRng* objects_from) {
  no_thread_runs_.emplace_back(
      taken.generator, objects_from, taken.context, running_,
      procedure_in_force_.load(std::memory_order_relaxed));
  running_ = &no_thread_runs_.back().state;
  procedure_in_force_.store(
      &no_thread_runs_.back().context.procedure, std::memory_order_relaxed);
}

auto Runtime::RunningContext() const -> const ExecutionContext& {
  if (const RuntimeProcess* const process = current_process_) {
    return process->Context();
  }
  if (no_thread_runs_.empty()) {
    throw InternalError(
        "Runtime::RunningContext: generated code runs only inside a process "
        "or on no thread, and neither is running");
  }
  return no_thread_runs_.back().context;
}

auto Runtime::RunningProcedure() const noexcept -> const ProcedureContext& {
  const RuntimeProcess* const process =
      current_process_.load(std::memory_order_relaxed);
  return process == nullptr
             ? *procedure_in_force_.load(std::memory_order_relaxed)
             : process->Context().procedure;
}

void Runtime::LeaveNoThread() {
  running_ = no_thread_runs_.back().displaced;
  procedure_in_force_.store(
      no_thread_runs_.back().displaced_procedure, std::memory_order_relaxed);
  no_thread_runs_.pop_back();
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
  RunRegion(support::Region::kPreponed);
  SlotPasses passes;
  // LRM 4.5: the active region set is run until it is empty, then the reactive
  // one, and the two again while either holds anything -- so what the reactive
  // set schedules into the active one waits until the reactive set is empty.
  while (current_slot_.FirstPending(
      support::Region::kActive, support::Region::kReNba)) {
    if (!DrainRegions(
            support::Region::kActive, support::Region::kObserved, passes) ||
        !DrainRegions(
            support::Region::kReactive, support::Region::kReNba, passes)) {
      return;
    }
  }
  // LRM 16.4.1: what is placed in the Postponed region while it runs, a final
  // deferred assertion's action among it, runs there in the same time slot.
  phase_ = Phase::kInPostponed;
  const bool settled = DrainRegions(
      support::Region::kPostponed, support::Region::kPostponed, passes);
  phase_ = Phase::kRunningRegions;
  if (!settled) {
    return;
  }
  if (std::holds_alternative<Running>(state_) && !current_slot_.Empty()) {
    // LRM 4.4.2.9.
    ReportDesignFailure(
        base::kNoSourceLocation,
        base::DiagMessage(
            "the postponed region scheduled work back into the time slot that "
            "ends with it"));
  }
}

auto Runtime::DrainRegions(
    support::Region first, support::Region last, SlotPasses& passes) -> bool {
  // What a region produces lands back in the slot, so the earliest one holding
  // anything is looked for again after each.
  while (const std::optional<support::Region> region =
             current_slot_.FirstPending(first, last)) {
    if (++passes.count > kMaxRegionPassesPerSlot) {
      ReportUnsettledSlot(passes.ran);
      return false;
    }
    if (passes.count > kMaxRegionPassesPerSlot - kNotedRegionPasses) {
      // What a pass runs is what its region holds as the pass begins, and a
      // procedure is noted from there because its run may be the one that ends
      // it.
      current_slot_[*region].activations.ForEach([&passes](
                                                     const QueuePlace& queued) {
        passes.ran.push_back(queued.activation->Process().Context().procedure);
      });
    }
    RunRegion(*region);
  }
  return true;
}

void Runtime::RunRegion(support::Region region) {
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
  //
  // What a deferred body entered to run on no thread ends with the effect
  // running it, however the effect returns.
  const auto run_effect =
      [this](const ProcedureContext& made_by, const auto& effect) {
        {
          const RunningAnEffect running(procedure_in_force_, made_by);
          const NoThreadRunsScope ends(*this);
          RunAsLanding(*this, effect);
        }
        TellExpired();
      };
  draining_.effects.Drain(run_effect);
  // A timer comes off its queue as it fires, so arming it again from what the
  // firing wakes queues a new event rather than moving this one. What it does
  // runs with no activation holding it, as a deferred effect does, so it is a
  // landing of its own too.
  while (TimerPlace* fired = draining_.timers.PopFront()) {
    {
      const RunningAnEffect running(
          procedure_in_force_, fired->timer->BoundBy());
      RunAsLanding(*this, [fired] { fired->timer->Fire(); });
    }
    TellExpired();
  }
  while (QueuePlace* queued = draining_.activations.PopFront()) {
    Activation* activation = queued->activation;
    ConsumeWait(activation);
    RunProcess(activation);
    TellExpired();
  }
  // What was kept for the end of this pass runs once everything the pass held
  // has, in the order it was kept, before anything the pass placed in the slot.
  while (!pass_end_effects_.Empty()) {
    draining_.effects.Append(pass_end_effects_);
    draining_.effects.Drain(run_effect);
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
  InFinalProcedure(RuntimeProcess& thread, const ExecutionContext& context)
      : thread_(&thread), left_(thread.EnterProcedure(context)) {
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
  ExecutionContext left_;
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
          .procedure = kNoProcedure,
          .seeds = &root.InitializationSeeds(),
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
      const InFinalProcedure in_it(thread, registered.context);
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

// What an execution runs in: the procedure it runs, the seeds of the instance
// whose text holds it (LRM 18.14.1), the region set the class of the instance
// whose code it evaluates states (LRM 24.3.1), and what created its thread. It
// belongs to no program; an initial procedure of one is made a member where it
// registers.
auto ContextIn(
    Scope* owning_scope, Scope* unit_instance, Scope* runs_as,
    base::SourceLocation written_at, ThreadCreator creator)
    -> ExecutionContext {
  return ExecutionContext{
      .procedure = {.scope = owning_scope, .written_at = written_at},
      .seeds = &unit_instance->InitializationSeeds(),
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
    case StartKind::kObserver:
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
    Coroutine<void> coroutine, base::SourceLocation written_at) {
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
    base::SourceLocation written_at) {
  RegisterStartingProcess(
      StartKind::kAlwaysProcedure, owning_scope, unit_instance, unit_instance,
      std::move(coroutine), written_at);
}

// An observer is woken inside each occurrence it waits for, and belongs to no
// lineage, since no procedure created it (LRM 9.6.1, 9.6.3).
void RegisterObservingProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    base::SourceLocation written_at) {
  Runtime& rt = AsRuntime(current_runtime());
  auto process = StartingProcess(
      ContextIn(
          owning_scope, unit_instance, unit_instance, written_at,
          CreatorOf(StartKind::kObserver))
          .WokenAtTheOccurrence(),
      std::move(coroutine));
  rt.QueueStart(StartKind::kObserver, process->TopActivation());
  rt.AdoptProcess(std::move(process));
}

void RegisterContinuousDriver(
    Scope* owning_scope, Scope* unit_instance, Scope* runs_as,
    Coroutine<void> coroutine, base::SourceLocation written_at) {
  RegisterStartingProcess(
      StartKind::kContinuousDriver, owning_scope, unit_instance, runs_as,
      std::move(coroutine), written_at);
}

// A program's initial procedure is one of its roots, which the program keeps,
// and every thread it starts is one of the program's (LRM 24.3, 24.7).
void RegisterInitialProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    base::SourceLocation written_at) {
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

void Exit(RuntimeEffects& runtime, base::SourceLocation at) {
  ProgramThreads* const program = AsRuntime(runtime).RunningContext().program;
  if (program == nullptr) {
    // LRM 24.7.
    const base::SourceLocationScope bound(at);
    runtime.Diagnostic().Report(
        Severity::kWarning,
        base::DiagMessage(
            "'$exit' is ignored, since this thread does not originate in an "
            "initial procedure of a program"));
    return;
  }
  program->Exit(runtime);
}

void RegisterTriggeredProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    base::SourceLocation written_at) {
  RegisterStartingProcess(
      StartKind::kTriggeredProcedure, owning_scope, unit_instance,
      unit_instance, std::move(coroutine), written_at);
}

void RegisterFinalProcedure(
    Scope* owning_scope, Scope* unit_instance,
    OwnedClosure<support::EffectSignature> call,
    base::SourceLocation written_at) {
  AsRuntime(current_runtime())
      .RegisterFinal(
          RegisteredFinal{
              .context = ContextIn(
                  owning_scope, unit_instance, unit_instance, written_at,
                  ThreadCreator::kAnotherConstruct),
              .call = std::move(call)});
}

void RunAssertionAction(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    base::SourceLocation written_at) {
  const ExecutionContext context = ContextIn(
      owning_scope, unit_instance, unit_instance, written_at,
      ThreadCreator::kAnotherConstruct);
  // No thread creates the action, so its generator is one the tick that
  // decided it hands over (LRM 18.14). At the end of the run nothing decides
  // it but the finished trace, and nothing hands one over, so it is the next
  // of its instance's initialization RNG, the actions there running in the
  // order their assertions registered (LRM 18.14.1).
  RuntimeEffects& effects = current_runtime();
  RunningState* const deciding = effects.TryRunning();
  AsRuntime(effects).RunAction(
      context, std::move(coroutine),
      deciding != nullptr ? deciding->HandOff()
                          : context.SplitInitializationRng());
}

void EnterScopeStaticInit(RuntimeEffects& runtime, Scope* unit_instance) {
  AsRuntime(runtime).EnterStaticInit(
      ExecutionContext{
          .procedure = {.scope = unit_instance},
          .seeds = &unit_instance->InitializationSeeds(),
          .creator = ThreadCreator::kAnotherConstruct});
}

void EnterNamespaceStaticInit(RuntimeEffects& runtime, const char* name) {
  Runtime& rt = AsRuntime(runtime);
  rt.EnterStaticInit(
      ExecutionContext{
          .procedure = kNoProcedure,
          .seeds = &rt.NamespaceSeeds(name),
          .creator = ThreadCreator::kAnotherConstruct});
}

auto ClaimNamespaceInitialization(RuntimeEffects& runtime, const char* name)
    -> std::int64_t {
  return AsRuntime(runtime).ClaimNamespaceInitialization(name) ? 1 : 0;
}

void LeaveStaticInit(RuntimeEffects& runtime) {
  AsRuntime(runtime).LeaveNoThread();
}

}  // namespace lyra::runtime
