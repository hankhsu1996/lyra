#pragma once

#include <array>
#include <csignal>
#include <cstddef>
#include <optional>
#include <vector>

#include "lyra/runtime/file.hpp"

namespace lyra::runtime {

class Runtime;

// For as long as one stands, it is the run's one owner of the signals the
// process is sent.
//
// A run stopped from outside -- by the keyboard's interrupt or by a request to
// terminate -- first delivers what the design had written and says where it
// was: the time, and where the procedure running at that moment is written. It
// then ends as the signal would have ended it, and no final procedure runs,
// since the simulation did not reach its end (LRM 9.2.3). A procedure that
// never stops to wait is legal text (LRM 9.2.2, 12.7.6) and cannot be told
// from one that is only taking long, so the run never decides for itself that
// it is stuck. Whoever is waiting on it decides, and this is what they are told
// when they do. A signal the program was started with ignored stays ignored,
// which is how a job put in the background is kept clear of the keyboard.
//
// A signal foreign code asked to be called back for (LRM 38.36.3) is instead
// trapped: the handler only notes it, and the run calls the callbacks at its
// next safe point, between two region passes. While any such callback stands
// the signal is theirs, a stop signal's report included, and once none does it
// is handled as it was before. A signal the system lets no program catch, and
// one that reports a fault of the instruction that raised it, which no program
// can carry on past, is none foreign code can be called back for.
class SignalWatcher {
 public:
  explicit SignalWatcher(Runtime& run);
  ~SignalWatcher();

  SignalWatcher(const SignalWatcher&) = delete;
  auto operator=(const SignalWatcher&) -> SignalWatcher& = delete;
  SignalWatcher(SignalWatcher&&) = delete;
  auto operator=(SignalWatcher&&) -> SignalWatcher& = delete;

  // Traps `signal` for a callback, answering false where it cannot be.
  auto Watch(int signal) -> bool;
  // A callback trapping `signal` ended.
  void Unwatch(int signal);

  // The signals noted since this was last asked, each once.
  static auto TakeNoted() -> std::vector<int>;

 private:
  static void SayWhereTheRunWas(int signal);
  static void Note(int signal);

  // The signals a callback can trap, by number, which a word of bits holds.
  static constexpr std::size_t kTrappable = 64;

  Runtime* run_;
  // What each stop signal did before this stood, for the ones it took over.
  std::array<std::optional<struct sigaction>, kStopSignals.size()> displaced_;
  // How many callbacks trap each signal, and what it did before the first.
  std::array<int, kTrappable> trapping_{};
  std::array<std::optional<struct sigaction>, kTrappable> before_trapped_;
};

}  // namespace lyra::runtime
