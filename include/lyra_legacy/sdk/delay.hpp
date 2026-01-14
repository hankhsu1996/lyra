#pragma once

#include <coroutine>
#include <cstdint>

namespace lyra::sdk {

class Scheduler;

// Thread-local scheduler pointer for coroutines to access
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline thread_local Scheduler* current_scheduler = nullptr;

// NOLINTBEGIN(readability-identifier-naming)
// Coroutine awaitable requires specific naming convention from C++ standard

/// ZeroDelay - schedules to Inactive region (same time slot)
/// Used for #0 delays per IEEE 1800 Section 4.4
class ZeroDelay {
 public:
  static auto await_ready() -> bool {
    return false;
  }

  void await_suspend(std::coroutine_handle<> handle) const;

  static void await_resume() {
  }
};

/// Delay - schedules to delay queue (future time slot)
/// For zero delays, use ZeroDelay instead
class Delay {
 public:
  explicit Delay(uint64_t amount) : amount_(amount) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  void await_suspend(std::coroutine_handle<> handle) const;

  static void await_resume() {
  }

  [[nodiscard]] auto Amount() const -> uint64_t {
    return amount_;
  }

 private:
  uint64_t amount_;
};

// NOLINTEND(readability-identifier-naming)

}  // namespace lyra::sdk
