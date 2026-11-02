#include "lyra/runtime/interrupt_report.hpp"

#include <array>
#include <atomic>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <string_view>
#include <unistd.h>

#include "lyra/base/source_location.hpp"
#include "lyra/base/write_all.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/runtime.hpp"

namespace lyra::runtime {

namespace {

// The run a signal arriving now is about. A handler is handed nothing but the
// signal's number, so the run it speaks for is found here.
auto ReportedRun() noexcept -> std::atomic<const Runtime*>& {
  static constinit std::atomic<const Runtime*> run = nullptr;
  return run;
}

// Everything below runs inside a signal handler, where only what POSIX lists
// as async-signal-safe may be called: no allocation, no stream, no formatting
// library. The text goes straight to the error stream's descriptor.
void Write(std::string_view text) noexcept {
  base::WriteAll(STDERR_FILENO, text);
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

}  // namespace

InterruptReport::InterruptReport(const Runtime& run) : run_(&run) {
  ReportedRun().store(&run);
  for (std::size_t i = 0; i < kStopSignals.size(); ++i) {
    struct sigaction standing{};
    if (sigaction(kStopSignals.at(i), nullptr, &standing) != 0 ||
        standing.sa_handler == SIG_IGN) {
      continue;
    }
    // The handler stands for one delivery: what it raises once it has spoken
    // then meets the signal's own action.
    struct sigaction report{};
    report.sa_handler = &InterruptReport::SayWhereTheRunWas;
    report.sa_flags = SA_RESETHAND;
    sigemptyset(&report.sa_mask);
    if (sigaction(kStopSignals.at(i), &report, nullptr) == 0) {
      displaced_.at(i) = standing;
    }
  }
}

InterruptReport::~InterruptReport() {
  for (std::size_t i = 0; i < kStopSignals.size(); ++i) {
    if (displaced_.at(i)) {
      sigaction(kStopSignals.at(i), &*displaced_.at(i), nullptr);
    }
  }
  // A second run constructed in the same process before this one ended has
  // taken the handler's attention; it keeps it.
  const Runtime* expected = run_;
  ReportedRun().compare_exchange_strong(expected, nullptr);
}

void InterruptReport::SayWhereTheRunWas(int signal) {
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
      Write(": note: this procedure was running\n");
    }
  }
  // Blocked while this handler runs, so it is delivered as the handler returns.
  std::raise(signal);
}

}  // namespace lyra::runtime
