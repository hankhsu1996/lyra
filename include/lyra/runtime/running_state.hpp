#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/source_location.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/import_call_chain.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

class ProgramThreads;
class RuntimeProcess;
class ReadReport;
class Scope;

// Where an execution woken by what it waits for is placed: in the Active region
// of its region set, in the slot the occurrence happened in, as every thread
// is; or at the occurrence itself, running before the execution that made it
// occur goes on, as a callback an occurrence invokes does. A covergroup's
// clocking event is the second: the group is sampled as it occurs, as though
// the process triggering it called `sample` (LRM 19.3).
enum class WakePlacement : std::uint8_t {
  kActiveRegion,
  kAtTheOccurrence,
};

// What created a thread, which is what the standard asks before it lets process
// control act on the thread (LRM 9.7) and before it lets a function the thread
// calls schedule what takes effect after the call returns (LRM 13.4.4): both
// are allowed only of a thread an initial or always procedure created, or a
// fork of one. Code that runs on no thread -- a static initialization, a
// deferred body -- counts as another construct.
enum class ThreadCreator : std::uint8_t {
  kInitialOrAlwaysProcedure,
  kAnotherConstruct,
};

// Whether an initial or always procedure, or a fork of one, created the thread
// -- the threads process control may act on (LRM 9.7) and whose function calls
// may schedule past their return (LRM 13.4.4).
[[nodiscard]] constexpr auto CreatedByInitialOrAlways(ThreadCreator creator)
    -> bool {
  switch (creator) {
    case ThreadCreator::kInitialOrAlwaysProcedure:
      return true;
    case ThreadCreator::kAnotherConstruct:
      return false;
  }
  return false;
}

// The procedure the code running belongs to, as a report names it. It outlives
// the execution running it, since what the procedure deferred reports for it
// after it has ended.
struct ProcedureContext {
  // The scope a report raised while it runs names -- a run-time warning or
  // error, and the note on a slot that never settles. Borrowed, and held for
  // the run. Null for a namespace's initializer, which has no scope.
  Scope* scope = nullptr;
  // Where the source writes what it runs: a constant of the program's, which
  // lasts the whole run, and no place where no source text states one. Read
  // as it is, because the one reader that cannot wait for a safe moment -- the
  // run being stopped from outside -- may neither allocate nor call anything
  // that does.
  base::SourceLocation written_at{};

  auto operator<=>(const ProcedureContext&) const
      -> std::strong_ordering = default;
};

// What belongs to no procedure: the run's own work, where no scope is reported.
inline constexpr ProcedureContext kNoProcedure{};

// What an execution runs in, which one it starts carries over, all but where a
// wake places it: that is chosen where an execution is started. A thread runs
// one procedure, except the thread the final procedures are called from, which
// takes on each one's of these as it calls it (LRM 9.2.3).
struct ExecutionContext {
  ProcedureContext procedure;
  // The initialization RNG of the module, interface or program instance, or of
  // the package, its declaration belongs to, which seeds the static processes
  // and static initializers there and the objects those initializers make
  // (LRM 18.14.1). Borrowed, and held for the run.
  InitializationRng* seeds = nullptr;
  // The region set the execution is placed in, which is the one its thread
  // started in, whatever the code it runs was declared in (LRM 24.3.1, 24.5).
  RegionSet regions = kActiveRegionSet;
  // Where it is placed once a wait it stops at is satisfied.
  WakePlacement wake = WakePlacement::kActiveRegion;
  // The program whose initial procedure the thread originates in, which a
  // `$exit` it calls ends (LRM 24.7). Borrowed, and held for the run. Null for
  // every other execution.
  ProgramThreads* program = nullptr;
  // What created the thread the execution belongs to, which a fork carries on
  // to the threads it starts (LRM 9.7, 13.4.4).
  ThreadCreator creator = ThreadCreator::kAnotherConstruct;

  // The context of a root thread of `program`, whose initial procedure it is,
  // and so of every thread it starts (LRM 24.3, 24.7).
  [[nodiscard]] auto InProgram(ProgramThreads& program) const
      -> ExecutionContext {
    ExecutionContext root = *this;
    root.program = &program;
    return root;
  }

  // The context of an execution this one starts: placed in this one's region
  // set and woken as every thread is.
  [[nodiscard]] auto ForStarted() const -> ExecutionContext {
    ExecutionContext started = *this;
    started.wake = WakePlacement::kActiveRegion;
    return started;
  }

  // The same for an execution of no lineage, which is no thread a `$exit`
  // ends (LRM 24.7) and none a procedure created.
  [[nodiscard]] auto WithoutLineage() const -> ExecutionContext {
    ExecutionContext detached = ForStarted();
    detached.program = nullptr;
    detached.creator = ThreadCreator::kAnotherConstruct;
    return detached;
  }

  // The same, for one woken at the occurrence it waits for.
  [[nodiscard]] auto WokenAtTheOccurrence() const -> ExecutionContext {
    ExecutionContext observing = WithoutLineage();
    observing.wake = WakePlacement::kAtTheOccurrence;
    return observing;
  }

  // The next generator of the initialization RNG here (LRM 18.14.1): a static
  // process's, a static initializer's, and that of an assertion's action the
  // end of the run starts, which nothing hands one over to.
  [[nodiscard]] auto SplitInitializationRng() const -> DrawRng {
    return seeds->Split();
  }
};

// What is in force while something runs. Five things are: the generator a
// randomization call draws from (LRM 18.14), the disable targets it is inside
// (LRM 9.6.2), the DPI-C import calls it is inside (LRM 35.5.3), the
// evaluation of a waited expression it is making, whose report an exported
// function foreign code calls reports into (LRM 9.4.2), and whether it ended
// the run (LRM 20.2). None is ever handed to the code that reads it -- a
// subroutine body is compiled once and reached from more than one runner, so
// each is read from whatever is running rather than passed in.
//
// A process owns one for its whole life, so two foreign calls suspended on
// different processes never share a chain. Code that runs on no thread owns
// one for its single run, which is why this is not state of a process: a
// variable declaration assignment runs before any procedure starts (LRM 10.5,
// 26.2), and a `$strobe` argument is evaluated after every process of its time
// step has run (LRM 21.2.2), and both can reach every one of them.
//
// The generator is not among what this owns. A process's is the generator of
// the object the process is, which its `srandom()` seeds (LRM 18.13.3), so it
// survives a suspension with the process; code on no thread keeps its own
// beside this.
struct RunningState {
  // What runs draws from `generator`, and an object it makes takes the next
  // split of `object_seeds` where there is one -- a static initializer's
  // object is seeded from its instance's initialization RNG (LRM 18.14.1) --
  // and of `generator` otherwise, as a thread seeds the objects it makes.
  // `owner` is the process this is the state of, or null for code on no
  // thread.
  RunningState(
      DrawRng& generator, InitializationRng* object_seeds,
      RuntimeProcess* owner)
      : generator(&generator), object_seeds(object_seeds), targets(owner) {
  }
  RunningState(const RunningState&) = delete;
  auto operator=(const RunningState&) -> RunningState& = delete;
  RunningState(RunningState&&) = delete;
  auto operator=(RunningState&&) -> RunningState& = delete;
  ~RunningState() = default;

  // The generator of an object made now (LRM 18.14.1).
  [[nodiscard]] auto ObjectGenerator() const -> DrawRng {
    return object_seeds != nullptr ? object_seeds->Split() : generator->Split();
  }

  // The generator of work handed over now to run where no thread runs it -- a
  // `$strobe`'s arguments, an update waiting on an event, an assertion's
  // action. It depends only on what runs and how many it has handed over, so
  // the order other threads run in does not move it (LRM 18.14.2), and handing
  // it over moves nothing what runs draws next (LRM 18.14).
  [[nodiscard]] auto HandOff() const -> DrawRng {
    return generator->HandOff();
  }

  // Never null.
  DrawRng* generator;
  // Null where an object takes its generator from `generator`.
  InitializationRng* object_seeds;
  EnclosingTargets targets;
  ImportCallChain import_calls;
  // Null where no evaluation is in progress.
  ReadReport* evaluation_report = nullptr;
  // Whether this ended the run -- by a task that ends it, by a `vpi_control`
  // carried out as an import returned, or by an error that ended it -- so it
  // leaves at the next point that asks, and nothing of the design runs in it
  // again (LRM 20.2). Unlike a disable or a kill, it puts no foreign frame
  // beneath it in the disabled state (LRM 35.9), since the run does not go on
  // past it.
  bool ended_the_run = false;
};

// What the design's code runs in where no thread runs it: a static
// initialization, before any procedure starts, and a deferred body that
// evaluates what the source wrote once its region runs it. It answers what the
// code asks of what is running as a process answers it, from state of its own,
// and carries what it displaced so that leaving restores whatever was running
// before. Its place never moves, since its state draws from the generator
// beside it.
struct NoThreadRun {
  NoThreadRun(
      DrawRng generator, InitializationRng* object_seeds,
      const ExecutionContext& context, RunningState* displaced,
      const ProcedureContext* displaced_procedure)
      : generator(generator),
        state(this->generator, object_seeds, nullptr),
        context(context),
        displaced(displaced),
        displaced_procedure(displaced_procedure) {
  }
  NoThreadRun(const NoThreadRun&) = delete;
  auto operator=(const NoThreadRun&) -> NoThreadRun& = delete;
  NoThreadRun(NoThreadRun&&) = delete;
  auto operator=(NoThreadRun&&) -> NoThreadRun& = delete;
  ~NoThreadRun() = default;

  DrawRng generator;
  RunningState state;
  // What the code runs in, which a deferred assertion it reaches takes its
  // action's context from. No initial or always procedure created it and it
  // belongs to no program.
  ExecutionContext context;
  // Null where nothing was running before this one started.
  RunningState* displaced = nullptr;
  // The procedure in force before this one started.
  const ProcedureContext* displaced_procedure;
};

// What a deferred body that evaluates what the source wrote takes where it is
// made, to run in once its region runs it: the context an execution of no
// lineage started there would run in, and the generator what runs there hands
// over to it. It holds nothing of the execution that made it, which may have
// ended by then.
struct NoThreadContext {
  // Takes the body on as what runs on no thread in the runtime attached to
  // this thread, until the effect running the body returns.
  void Enter() const;

  ExecutionContext context;
  DrawRng generator{RandomSeed{0}};
};

}  // namespace lyra::runtime
