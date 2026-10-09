#include "lyra/compiler/stack.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <future>
#include <mutex>
#include <pthread.h>
#include <span>
#include <string_view>
#include <sys/resource.h>
#include <sys/types.h>
#include <unistd.h>
#include <utility>

#include "lyra/base/internal_error.hpp"

namespace lyra::compiler {

namespace {

// A run of ten thousand operands takes between half of this and all of it in
// an unoptimized build of the compiler.
constexpr std::size_t kCompileStackSize = std::size_t{64} << 20;

constexpr std::size_t kSignalStackSize = std::size_t{64} << 10;

// How far around the end of a stack a faulting address is taken to mean the
// stack ran out: the pages kept unmapped below a stack, and a frame large
// enough to step over them.
constexpr std::uintptr_t kBelowStackEnd = std::uintptr_t{2} << 20;
constexpr std::uintptr_t kAboveStackEnd = std::uintptr_t{64} << 10;

constexpr std::string_view kOutOfStack =
    "lyra: the compiler ran out of stack. An expression in the design is "
    "longer, or nested more deeply, than lyra supports yet.\n";

// The lowest address of the stack the calling thread runs on, once the thread
// has noted it.
auto StackEnd() -> std::uintptr_t& {
  thread_local std::uintptr_t end = 0;
  return end;
}

// What handled a fault before the handler below was installed, so a fault it
// has spoken about ends the process the way it would have.
auto PreviousFaultAction() -> struct sigaction& {
  static struct sigaction previous{};
  return previous;
}

// Says the stack ran out where that is what the fault is, then hands the
// signal back: returning runs the faulting instruction again, and whatever
// handled the signal before this ends the process. A handler that is
// given the fault's address takes a third argument, which this one does not
// read.
// NOLINTNEXTLINE(readability-named-parameter)
void ReportStackOverflow(int signal, siginfo_t* info, void*) {
  const auto fault = std::bit_cast<std::uintptr_t>(info->si_addr);
  const std::uintptr_t end = StackEnd();
  if (end != 0 && fault + kBelowStackEnd >= end &&
      fault < end + kAboveStackEnd) {
    std::string_view unwritten = kOutOfStack;
    while (!unwritten.empty()) {
      const ssize_t written =
          ::write(STDERR_FILENO, unwritten.data(), unwritten.size());
      if (written <= 0) break;
      unwritten.remove_prefix(static_cast<std::size_t>(written));
    }
  }
  ::sigaction(signal, &PreviousFaultAction(), nullptr);
}

// A fault from running out of stack is delivered on a stack of its own, per
// thread, so each thread that compiles names one and notes where its own stack
// ends. A thread that already has one keeps it.
void WatchThisThreadsStack(std::span<std::byte> signal_stack) {
  static std::once_flag installed;
  std::call_once(installed, [] {
    struct sigaction action{};
    action.sa_sigaction = ReportStackOverflow;
    action.sa_flags = SA_SIGINFO | SA_ONSTACK;
    ::sigemptyset(&action.sa_mask);
    ::sigaction(SIGSEGV, &action, &PreviousFaultAction());
  });

  stack_t current{};
  ::sigaltstack(nullptr, &current);
  if ((current.ss_flags & SS_DISABLE) != 0) {
    const stack_t own{
        .ss_sp = signal_stack.data(),
        .ss_flags = 0,
        .ss_size = signal_stack.size()};
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

}  // namespace

void GiveStartingThreadCompileStack() {
  static std::array<std::byte, kSignalStackSize> signal_stack;

  rlimit limit{};
  if (::getrlimit(RLIMIT_STACK, &limit) == 0 &&
      limit.rlim_cur != RLIM_INFINITY && limit.rlim_cur < kCompileStackSize) {
    limit.rlim_cur = limit.rlim_max == RLIM_INFINITY
                         ? rlim_t{kCompileStackSize}
                         : std::min(rlim_t{kCompileStackSize}, limit.rlim_max);
    ::setrlimit(RLIMIT_STACK, &limit);
  }
  WatchThisThreadsStack(signal_stack);
}

StackThread::StackThread(std::function<void()> work)
    : work_(std::move(work)),
      done_(work_.get_future()),
      signal_stack_(kSignalStackSize) {
  pthread_attr_t attributes;
  ::pthread_attr_init(&attributes);
  ::pthread_attr_setstacksize(&attributes, kCompileStackSize);
  const int refused = ::pthread_create(&thread_, &attributes, Run, this);
  ::pthread_attr_destroy(&attributes);
  if (refused != 0) {
    throw InternalError("compiler: the host refused a thread to compile on");
  }
}

StackThread::~StackThread() {
  if (!joined_) {
    ::pthread_join(thread_, nullptr);
  }
}

void StackThread::Join() {
  ::pthread_join(thread_, nullptr);
  joined_ = true;
  done_.get();
}

auto StackThread::Run(void* self) -> void* {
  auto& thread = *static_cast<StackThread*>(self);
  WatchThisThreadsStack(thread.signal_stack_);
  thread.work_();
  return nullptr;
}

}  // namespace lyra::compiler
