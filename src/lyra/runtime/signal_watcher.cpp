#include "lyra/runtime/signal_watcher.hpp"

#include <array>
#include <atomic>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <string_view>
#include <unistd.h>
#include <vector>

#include "lyra/base/source_location.hpp"
#include "lyra/base/write_all.hpp"
#include "lyra/runtime/file.hpp"
#include "lyra/runtime/finished_output.hpp"
#include "lyra/runtime/runtime.hpp"

namespace lyra::runtime {

namespace {

// The run a signal arriving now is about. A handler is handed nothing but the
// signal's number, so the run it speaks for is found here.
auto ReportedRun() noexcept -> std::atomic<const Runtime*>& {
  static constinit std::atomic<const Runtime*> run = nullptr;
  return run;
}

// The trapped signals that arrived and have not been called back yet, one bit
// each by number.
auto Noted() noexcept -> std::atomic<std::uint64_t>& {
  static constinit std::atomic<std::uint64_t> noted = 0;
  return noted;
}

// Everything below runs inside a signal handler, where only what POSIX lists
// as async-signal-safe may be called: no allocation, no stream, no formatting
// library. The text goes straight to the error stream's descriptor.
void Write(std::string_view text) noexcept {
  base::WriteAsMuchAsTaken(STDERR_FILENO, text);
}

void WriteDecimal(std::uint64_t value) noexcept {
  // The digits of the largest 64-bit value.
  std::array<char, 20> digits{};
  std::size_t first = digits.size();
  while (true) {
    digits.at(--first) = static_cast<char>('0' + (value % 10));
    value /= 10;
    if (value == 0) {
      break;
    }
  }
  Write(std::string_view{digits.data(), digits.size()}.substr(first));
}

// A signal reporting a fault of the instruction that raised it: returning from
// its handler raises it again, so no program carries on past one.
auto ReportsAFault(int signal) -> bool {
  return signal == SIGSEGV || signal == SIGBUS || signal == SIGFPE ||
         signal == SIGILL;
}

}  // namespace

SignalWatcher::SignalWatcher(Runtime& run) : run_(&run) {
  ReportedRun().store(&run);
  run.signals_ = this;
  for (std::size_t i = 0; i < kStopSignals.size(); ++i) {
    struct sigaction standing{};
    if (sigaction(kStopSignals.at(i), nullptr, &standing) != 0 ||
        standing.sa_handler == SIG_IGN) {
      continue;
    }
    // The handler stands for one delivery: what it raises once it has spoken
    // then meets the signal's own action.
    struct sigaction report{};
    report.sa_handler = &SignalWatcher::SayWhereTheRunWas;
    report.sa_flags = SA_RESETHAND;
    sigemptyset(&report.sa_mask);
    if (sigaction(kStopSignals.at(i), &report, nullptr) == 0) {
      displaced_.at(i) = standing;
    }
  }
}

SignalWatcher::~SignalWatcher() {
  for (std::size_t signal = 0; signal < kTrappable; ++signal) {
    if (trapping_.at(signal) > 0 && before_trapped_.at(signal)) {
      sigaction(
          static_cast<int>(signal), &*before_trapped_.at(signal), nullptr);
    }
  }
  for (std::size_t i = 0; i < kStopSignals.size(); ++i) {
    if (displaced_.at(i)) {
      sigaction(kStopSignals.at(i), &*displaced_.at(i), nullptr);
    }
  }
  run_->signals_ = nullptr;
  // A second run constructed in the same process before this one ended has
  // taken the handler's attention; it keeps it.
  const Runtime* expected = run_;
  ReportedRun().compare_exchange_strong(expected, nullptr);
}

auto SignalWatcher::Watch(int signal) -> bool {
  if (signal <= 0 || static_cast<std::size_t>(signal) >= kTrappable ||
      ReportsAFault(signal)) {
    return false;
  }
  int& trapping = trapping_.at(static_cast<std::size_t>(signal));
  if (trapping == 0) {
    struct sigaction note{};
    note.sa_handler = &SignalWatcher::Note;
    note.sa_flags = SA_RESTART;
    sigemptyset(&note.sa_mask);
    struct sigaction before{};
    if (sigaction(signal, &note, &before) != 0) {
      return false;
    }
    before_trapped_.at(static_cast<std::size_t>(signal)) = before;
  }
  ++trapping;
  return true;
}

void SignalWatcher::Unwatch(int signal) {
  int& trapping = trapping_.at(static_cast<std::size_t>(signal));
  if (--trapping > 0) return;
  std::optional<struct sigaction>& before =
      before_trapped_.at(static_cast<std::size_t>(signal));
  if (before) sigaction(signal, &*before, nullptr);
  before.reset();
}

auto SignalWatcher::TakeNoted() -> std::vector<int> {
  std::vector<int> signals;
  const std::uint64_t noted = Noted().exchange(0);
  for (std::size_t signal = 0; signal < kTrappable; ++signal) {
    if ((noted & (std::uint64_t{1} << signal)) != 0) {
      signals.push_back(static_cast<int>(signal));
    }
  }
  return signals;
}

void SignalWatcher::Note(int signal) {
  Noted().fetch_or(std::uint64_t{1} << static_cast<unsigned>(signal));
}

void SignalWatcher::SayWhereTheRunWas(int signal) {
  DeliverFinishedOutput();
  if (const Runtime* run = ReportedRun().load(); run != nullptr) {
    Write("note: interrupted at time ");
    WriteDecimal(run->now_.load());
    Write("\n");
    const base::SourceLocation written_at = run->RunningProcedure().written_at;
    if (written_at.IsValid()) {
      Write(written_at.File());
      Write(":");
      WriteDecimal(written_at.Line());
      Write(":");
      WriteDecimal(written_at.Column());
      Write(": note: this process was running\n");
    }
  }
  // Its handler is gone, so the signal's own action ends the process: at once
  // where finishing the run's dumps already let the stop signals through, and
  // otherwise as this handler returns, the signal being blocked while it runs.
  std::raise(signal);
}

}  // namespace lyra::runtime
