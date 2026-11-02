#pragma once

#include <stdexcept>
#include <string_view>
#include <utility>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/source_location.hpp"

namespace lyra {

namespace base {

// The place bound now, where `what` is being raised, reported or kept for the
// design. Every way into the library that can fail the design opens a scope
// with its place, so none bound is a defect of the tool and not of the design,
// and is an internal error naming `what`.
[[nodiscard]] auto BoundSourceLocation(std::string_view what) -> SourceLocation;

}  // namespace base

// A failure of the simulated design rather than of the compiler: the source is
// legal SystemVerilog, and running it reached something the LRM defines as a
// run-time error, or an operation this simulator does not yet carry out. The
// message names the SystemVerilog-level cause, because the reader's next step
// is to change their design or to ask for the missing support. That is what
// separates it from an invariant violation, whose reader can only report a
// compiler bug.
//
// It carries where in the design the failing operation was written, read off
// what is bound as it is raised and kept, so the report names the statement to
// change. A raise the run makes for no operation of the design says so, and
// keeps no place.
//
// Raising one leaves the body it was raised in, because there is no value the
// evaluation could have carried on with. What it settles is the run: the
// activation's landing reports it at the severity the tool chose for the
// condition and ends the simulation the way `$fatal` does (LRM 20.10).
class SimulationError final : public std::runtime_error {
 public:
  explicit SimulationError(base::DiagMessage message);
  SimulationError(
      base::NoSourceLocation no_location, base::DiagMessage message);

  template <typename... Args>
  explicit SimulationError(base::DiagFormatFor<Args...> format, Args&&... args)
      : SimulationError(
            base::DiagMessage(format, std::forward<Args>(args)...)) {
  }
  template <typename... Args>
  SimulationError(
      base::NoSourceLocation no_location, base::DiagFormatFor<Args...> format,
      Args&&... args)
      : SimulationError(
            no_location,
            base::DiagMessage(format, std::forward<Args>(args)...)) {
  }

  SimulationError(const SimulationError&) = default;
  auto operator=(const SimulationError&) -> SimulationError& = default;
  SimulationError(SimulationError&&) = default;
  auto operator=(SimulationError&&) -> SimulationError& = default;
  // Defined in this class's own source file, because a class whose virtual
  // functions are all written in a header is emitted into every translation
  // unit that builds one -- and a thrown type reaches every unit that can
  // raise it.
  ~SimulationError() override;

  // Where in the design the failing operation was written, no place for a
  // raise of the run's own.
  [[nodiscard]] auto At() const -> base::SourceLocation {
    return at_;
  }

 private:
  base::SourceLocation at_{};
};

}  // namespace lyra
