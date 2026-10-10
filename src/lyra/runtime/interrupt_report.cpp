#include "lyra/runtime/interrupt_report.hpp"

#include <array>
#include <atomic>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <string_view>
#include <unistd.h>

#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_process.hpp"

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
  while (!text.empty()) {
    const ssize_t written = ::write(STDERR_FILENO, text.data(), text.size());
    if (written <= 0) {
      return;
    }
    text.remove_prefix(static_cast<std::size_t>(written));
  }
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

InterruptReport::InterruptReport(const Runtime& run) {
  ReportedRun().store(&run);
  for (std::size_t i = 0; i < kSignals.size(); ++i) {
    struct sigaction standing{};
    if (sigaction(kSignals.at(i), nullptr, &standing) != 0 ||
        standing.sa_handler == SIG_IGN) {
      continue;
    }
    // The handler stands for one delivery: what it raises once it has spoken
    // then meets the signal's own action.
    struct sigaction report{};
    report.sa_handler = &InterruptReport::SayWhereTheRunWas;
    report.sa_flags = SA_RESETHAND;
    sigemptyset(&report.sa_mask);
    if (sigaction(kSignals.at(i), &report, nullptr) == 0) {
      displaced_.at(i) = standing;
    }
  }
}

InterruptReport::~InterruptReport() {
  for (std::size_t i = 0; i < kSignals.size(); ++i) {
    if (displaced_.at(i)) {
      sigaction(kSignals.at(i), &*displaced_.at(i), nullptr);
    }
  }
  ReportedRun().store(nullptr);
}

void InterruptReport::SayWhereTheRunWas(int signal) {
  if (const Runtime* run = ReportedRun().load(); run != nullptr) {
    Write("note: interrupted at time ");
    WriteDecimal(run->now_.load());
    Write("\n");
    const RuntimeProcess* running =
        run->current_process_.load(std::memory_order_relaxed);
    const std::string_view written_at =
        running == nullptr ? std::string_view{} : running->WrittenAt();
    if (!written_at.empty()) {
      Write("note: ");
      Write(written_at);
      Write(": this procedure was running\n");
    }
  }
  // Blocked while this handler runs, so it is delivered as the handler returns.
  std::raise(signal);
}

}  // namespace lyra::runtime
