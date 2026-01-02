#pragma once

#include <coroutine>
#include <cstdint>

namespace lyra::sdk {

class Scheduler;

// Forward declaration - defined in delay.hpp
extern thread_local Scheduler* current_scheduler;

// NOLINTBEGIN(readability-identifier-naming)
// Coroutine awaitable requires specific naming convention from C++ standard

// Awaitable for waiting on a positive edge (0 -> 1 transition)
class WaitPosedge {
 public:
  explicit WaitPosedge(bool* var) : var_(var) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  void await_suspend(std::coroutine_handle<> handle) const;

  static void await_resume() {
  }

 private:
  bool* var_;
};

// Awaitable for waiting on a negative edge (1 -> 0 transition)
class WaitNegedge {
 public:
  explicit WaitNegedge(bool* var) : var_(var) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  void await_suspend(std::coroutine_handle<> handle) const;

  static void await_resume() {
  }

 private:
  bool* var_;
};

// NOLINTEND(readability-identifier-naming)

}  // namespace lyra::sdk
