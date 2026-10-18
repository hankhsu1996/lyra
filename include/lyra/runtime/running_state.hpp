#pragma once

#include "lyra/runtime/import_call_chain.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

class ProgramThreads;
class Scope;

// What an execution runs in, which one it starts carries over unchanged.
struct ExecutionContext {
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
  // The region set the execution is placed in, which is the one its thread
  // started in, whatever the code it runs was declared in (LRM 24.3.1, 24.5).
  RegionSet regions = kActiveRegionSet;
  // The program whose initial procedure the thread originates in, which a
  // `$exit` it calls ends (LRM 24.7). Borrowed, and held for the run. Null for
  // every other execution.
  ProgramThreads* program = nullptr;

  // The context of a root thread of `program`, whose initial procedure it is,
  // and so of every thread it starts (LRM 24.3, 24.7).
  [[nodiscard]] auto InProgram(ProgramThreads& program) const
      -> ExecutionContext {
    ExecutionContext root = *this;
    root.program = &program;
    return root;
  }

  // The context of an execution of no lineage started from this one: placed
  // where this one is, but no thread a `$exit` ends (LRM 24.7).
  [[nodiscard]] auto WithoutLineage() const -> ExecutionContext {
    ExecutionContext detached = *this;
    detached.program = nullptr;
    return detached;
  }

  // A generator for an execution nothing that is a thread creates here: the
  // next one of the initialization RNG (LRM 18.14.1).
  [[nodiscard]] auto SplitInitializationRng() const -> DrawRng {
    return seeds->Split();
  }
};

// What is in force while something runs. Two things are: the generator a
// randomization call draws from (LRM 18.14), and the DPI-C import calls this
// execution is inside (LRM 35.5.3). Neither is ever handed to the code that
// reads it -- a subroutine body is compiled once and reached from more than one
// runner, so both are read from whatever is running rather than passed in.
//
// A process owns one for its whole life, so its generator survives a suspension
// and two foreign calls suspended on different processes never share a chain. A
// static initialization owns one for its single run, which is why this is not
// state of a process: a variable declaration assignment runs before any
// procedure starts (LRM 10.5, 26.2) and can reach both of these.
struct RunningState {
  // What runs starts from the generator it is handed, inside no foreign call.
  explicit RunningState(DrawRng initial) : rng(initial) {
  }

  DrawRng rng;
  ImportCallChain import_calls;
};

// A `RunningState` a static initialization owns, carrying what it displaced so
// that leaving restores whatever was running before.
struct DisplacingState {
  RunningState state;
  // What the initialization runs in, which a deferred assertion it reaches
  // takes its action's context from.
  ExecutionContext context;
  // Null where nothing was running before this one started.
  RunningState* displaced = nullptr;
};

}  // namespace lyra::runtime
