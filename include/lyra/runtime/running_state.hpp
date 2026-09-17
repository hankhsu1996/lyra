#pragma once

#include "lyra/runtime/import_call_chain.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

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
  DrawRng rng;
  ImportCallChain import_calls;
};

// A `RunningState` a static initialization owns, carrying what it displaced so
// that leaving restores whatever was running before.
struct DisplacingState {
  RunningState state;
  // Null where nothing was running before this one started.
  RunningState* displaced = nullptr;
};

}  // namespace lyra::runtime
