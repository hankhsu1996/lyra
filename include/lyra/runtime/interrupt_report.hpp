#pragma once

#include <array>
#include <csignal>
#include <optional>

namespace lyra::runtime {

class Runtime;

// For as long as one stands, a run that is stopped from outside -- by the
// keyboard's interrupt or by a request to terminate -- first says where it was:
// the time, and where the procedure running at that moment is written. It then
// ends as the signal would have ended it, and no final procedure runs, since
// the simulation did not reach its end (LRM 9.2.3).
//
// A procedure that never stops to wait is legal text (LRM 9.2.2, 12.7.6) and
// cannot be told from one that is only taking long, so the run never decides
// for itself that it is stuck. Whoever is waiting on it decides, and this is
// what they are told when they do.
//
// A signal the program was started with ignored stays ignored, which is how a
// job put in the background is kept clear of the keyboard.
class InterruptReport {
 public:
  explicit InterruptReport(const Runtime& run);
  ~InterruptReport();

  InterruptReport(const InterruptReport&) = delete;
  auto operator=(const InterruptReport&) -> InterruptReport& = delete;
  InterruptReport(InterruptReport&&) = delete;
  auto operator=(InterruptReport&&) -> InterruptReport& = delete;

 private:
  static constexpr std::array kSignals{SIGINT, SIGTERM};

  static void SayWhereTheRunWas(int signal);

  // What each signal did before this stood, for the ones it took over.
  std::array<std::optional<struct sigaction>, kSignals.size()> displaced_;
};

}  // namespace lyra::runtime
