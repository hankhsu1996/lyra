#include "lyra/base/fault_watch.hpp"

#include <array>
#include <bit>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <mutex>
#include <pthread.h>
#include <span>
#include <string_view>
#include <unistd.h>

#include "lyra/base/write_all.hpp"

namespace lyra::base {

namespace {

// The signals a fault arrives as.
constexpr std::array kFaultSignals{SIGSEGV, SIGBUS, SIGFPE, SIGILL, SIGABRT};

// How far around the end of a stack a faulting address is taken to mean the
// stack overflowed: below it, as far as a large frame can step past the pages
// kept unmapped there, and a small margin above it.
constexpr std::uintptr_t kBelowStackEnd = std::uintptr_t{2} << 20;
constexpr std::uintptr_t kAboveStackEnd = std::uintptr_t{64} << 10;

// The lowest address of the stack the calling thread runs on, once the thread
// has noted it.
auto StackEnd() -> std::uintptr_t& {
  thread_local std::uintptr_t end = 0;
  return end;
}

// What an overflow is reported with.
auto OverflowMessage() -> std::string_view& {
  static std::string_view message;
  return message;
}

// What runs before the process ends, once something is installed.
auto BeforeEndingAction() -> BeforeEnding& {
  static BeforeEnding action = &NothingBeforeEnding;
  return action;
}

// What handled each fault signal before the handler below was installed, so
// a fault it has spoken about ends the process the way it would have.
auto PreviousFaultActions()
    -> std::array<struct sigaction, kFaultSignals.size()>& {
  static std::array<struct sigaction, kFaultSignals.size()> previous{};
  return previous;
}

// Runs what was installed to run before an ending, says the stack ran out
// where that is what the fault is, then hands the signal back: returning runs
// the faulting instruction again, and whatever handled the signal before this
// ends the process. An abort raises its signal again once a handler returns,
// which comes to the same. A handler that is given the fault's address takes
// a third argument, which this one does not read.
// NOLINTNEXTLINE(readability-named-parameter)
void ReportFault(int signal, siginfo_t* info, void*) {
  BeforeEndingAction()();
  const auto fault = std::bit_cast<std::uintptr_t>(info->si_addr);
  const std::uintptr_t end = StackEnd();
  if (signal == SIGSEGV && end != 0 && fault + kBelowStackEnd >= end &&
      fault < end + kAboveStackEnd) {
    WriteAll(STDERR_FILENO, OverflowMessage());
  }
  for (std::size_t i = 0; i < kFaultSignals.size(); ++i) {
    if (kFaultSignals.at(i) == signal) {
      ::sigaction(signal, &PreviousFaultActions().at(i), nullptr);
    }
  }
}

}  // namespace

void NothingBeforeEnding() noexcept {
}

void InstallFaultHandler(
    std::string_view overflow_message, BeforeEnding before_ending) {
  static std::once_flag installed;
  std::call_once(installed, [&] {
    OverflowMessage() = overflow_message;
    BeforeEndingAction() = before_ending;
    struct sigaction action{};
    action.sa_sigaction = ReportFault;
    action.sa_flags = SA_SIGINFO | SA_ONSTACK;
    ::sigemptyset(&action.sa_mask);
    for (std::size_t i = 0; i < kFaultSignals.size(); ++i) {
      ::sigaction(kFaultSignals.at(i), &action, &PreviousFaultActions().at(i));
    }
  });
}

void RegisterThreadStack(std::span<std::byte> alt_stack) {
  stack_t current{};
  ::sigaltstack(nullptr, &current);
  if ((current.ss_flags & SS_DISABLE) != 0) {
    const stack_t own{
        .ss_sp = alt_stack.data(), .ss_flags = 0, .ss_size = alt_stack.size()};
    ::sigaltstack(&own, nullptr);
  }

  pthread_attr_t attributes;
  if (::pthread_getattr_np(::pthread_self(), &attributes) != 0) {
    return;
  }
  void* lowest = nullptr;
  std::size_t size = 0;
  ::pthread_attr_getstack(&attributes, &lowest, &size);
  ::pthread_attr_destroy(&attributes);
  StackEnd() = std::bit_cast<std::uintptr_t>(lowest);
}

}  // namespace lyra::base
