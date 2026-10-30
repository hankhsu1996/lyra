#pragma once

#include <cstdint>

#include "lyra/runtime/import_call_chain.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

class ProgramThreads;
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
// fork of one. Static initialization runs on no thread, and counts as another
// construct.
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

// What the code an execution is running belongs to. A thread runs one
// procedure, except the thread the final procedures are called from, which
// runs each of them in turn and takes on each one's of these as it calls it
// (LRM 9.2.3).
struct ProcedureContext {
  // The scope a report raised while it runs names -- a run-time warning or
  // error, and the note on a slot that never settles. Borrowed, and outlives
  // the execution. Null for a namespace's initializer, which has no scope.
  Scope* scope = nullptr;
  // The initialization RNG of the module, interface or program instance, or of
  // the package, its declaration belongs to, which seeds whatever nothing that
  // is a thread creates there (LRM 18.14.1). Borrowed, and held for the run.
  InitializationRng* seeds = nullptr;
  // Where the source writes what it runs, as text that stands for the whole
  // run; empty where no source text states one. Plain characters, because the
  // one reader that cannot wait for a safe moment -- the run being stopped from
  // outside -- may neither allocate nor call anything that does.
  const char* written_at = "";
};

// What an execution runs in, which one it starts carries over, all but where a
// wake places it: that is chosen where an execution is started.
struct ExecutionContext {
  ProcedureContext procedure;
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

  // A generator for an execution nothing that is a thread creates here: the
  // next one of the initialization RNG (LRM 18.14.1).
  [[nodiscard]] auto SplitInitializationRng() const -> DrawRng {
    return procedure.seeds->Split();
  }
};

// What is in force while something runs. Three things are: the generator a
// randomization call draws from (LRM 18.14), the DPI-C import calls this
// execution is inside (LRM 35.5.3), and the evaluation of a waited expression
// it is making, whose report an exported function foreign code calls reports
// into (LRM 9.4.2). None is ever handed to the code that reads it -- a
// subroutine body is compiled once and reached from more than one runner, so
// each is read from whatever is running rather than passed in.
//
// A process owns one for its whole life, so two foreign calls suspended on
// different processes never share a chain. A static initialization owns one
// for its single run, which is why this is not state of a process: a variable
// declaration assignment runs before any procedure starts (LRM 10.5, 26.2) and
// can reach both of these.
//
// The generator is not among what this owns. A process's is the generator of
// the object the process is, which its `srandom()` seeds (LRM 18.13.3), so it
// survives a suspension with the process; a static initialization keeps its
// own beside this.
struct RunningState {
  // What runs draws from `generator`, inside no foreign call.
  explicit RunningState(DrawRng& generator) : generator(&generator) {
  }

  // Never null.
  DrawRng* generator;
  ImportCallChain import_calls;
  // Null where no evaluation is in progress.
  ReadReport* evaluation_report = nullptr;
};

// A `RunningState` a static initialization owns, carrying what it displaced so
// that leaving restores whatever was running before. Its place never moves,
// since its state draws from the generator beside it.
struct DisplacingState {
  DisplacingState(
      DrawRng initial, const ExecutionContext& context, RunningState* displaced)
      : generator(initial),
        state(generator),
        context(context),
        displaced(displaced) {
  }
  DisplacingState(const DisplacingState&) = delete;
  auto operator=(const DisplacingState&) -> DisplacingState& = delete;
  DisplacingState(DisplacingState&&) = delete;
  auto operator=(DisplacingState&&) -> DisplacingState& = delete;
  ~DisplacingState() = default;

  DrawRng generator;
  RunningState state;
  // What the initialization runs in, which a deferred assertion it reaches
  // takes its action's context from.
  ExecutionContext context;
  // Null where nothing was running before this one started.
  RunningState* displaced = nullptr;
};

}  // namespace lyra::runtime
