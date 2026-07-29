#pragma once

#include <stdexcept>
#include <string>

namespace lyra {

// A failure of the simulated design rather than of the compiler: the source is
// legal SystemVerilog, and running it reached something the LRM defines as a
// run-time error, or an operation this simulator does not yet carry out. The
// message names the SystemVerilog-level cause, because the reader's next step
// is to change their design or to ask for the missing support. That is what
// separates it from an invariant violation, whose reader can only report a
// compiler bug.
//
// It escapes the process that raised it and unwinds to the host boundary,
// which reports it and ends the simulation with a failing status.
class SimulationError final : public std::runtime_error {
 public:
  explicit SimulationError(std::string message);
};

}  // namespace lyra
