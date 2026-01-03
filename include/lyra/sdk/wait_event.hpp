#pragma once

#include <coroutine>
#include <cstdint>
#include <functional>
#include <vector>

namespace lyra::sdk {

class Scheduler;

// Forward declaration - defined in delay.hpp
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
extern thread_local Scheduler* current_scheduler;

// Edge kind for wait triggers (matches common::EdgeKind)
enum class EdgeKind { kAnyChange, kPosedge, kNegedge, kBothEdge };

// NOLINTBEGIN(readability-identifier-naming)
// Coroutine awaitable requires specific naming convention from C++ standard

// Awaitable for implicit events (LRM 9.4.2)
// An implicit event is a value change of an expression.
// Supports edge events (posedge/negedge/edge on LSB) and value change events.
template <typename T>
class ImplicitEvent {
 public:
  ImplicitEvent(T* var, EdgeKind kind) : var_(var), kind_(kind) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  // Defined in scheduler.hpp after Scheduler is complete
  void await_suspend(std::coroutine_handle<> handle) const;

  static void await_resume() {
  }

 private:
  T* var_;
  EdgeKind kind_;
};

// Trigger info for multi-variable waits
// Uses type-erased reader function for extensibility to any type/bit-width
struct TriggerInfo {
  std::function<int64_t()> read_value;  // Type-erased value reader
  EdgeKind kind;
  int64_t value;  // Captured value at registration time
};

// Awaitable for OR-combined implicit events (e.g., always_comb sensitivity
// list) Triggers when ANY of the watched variables change
class ImplicitEventOr {
 public:
  ImplicitEventOr(std::initializer_list<TriggerInfo> triggers)
      : triggers_(triggers) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  // Defined in scheduler.hpp after Scheduler is complete
  void await_suspend(std::coroutine_handle<> handle) const;

  static void await_resume() {
  }

 private:
  std::vector<TriggerInfo> triggers_;
};

// NOLINTEND(readability-identifier-naming)

}  // namespace lyra::sdk
