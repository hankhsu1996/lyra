#pragma once

#include <coroutine>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <vector>

namespace lyra::sdk {

class Scheduler;

// Forward declaration - defined in delay.hpp
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
extern thread_local Scheduler* current_scheduler;

// Edge kind for wait triggers (matches common::EdgeKind)
enum class EdgeKind { kAnyChange, kPosedge, kNegedge, kBothEdge };

template <typename T>
auto MakeTriggerChecker(T* var, EdgeKind kind) -> std::function<bool()> {
  if (kind == EdgeKind::kAnyChange) {
    return [var, prev = *var]() mutable {
      bool triggered = *var != prev;
      prev = *var;
      return triggered;
    };
  }
  // Edge triggers only make sense for types convertible to uint64_t
  if constexpr (std::is_convertible_v<T, uint64_t>) {
    return [var, kind, prev_bit = static_cast<uint64_t>(*var) & 1]() mutable {
      uint64_t new_bit = static_cast<uint64_t>(*var) & 1;
      bool triggered = false;
      switch (kind) {
        case EdgeKind::kPosedge:
          triggered = (prev_bit == 0 && new_bit == 1);
          break;
        case EdgeKind::kNegedge:
          triggered = (prev_bit == 1 && new_bit == 0);
          break;
        case EdgeKind::kBothEdge:
          triggered = (prev_bit != new_bit);
          break;
        default:
          break;
      }
      prev_bit = new_bit;
      return triggered;
    };
  } else {
    // Non-numeric types don't support edge triggers
    return []() { return false; };
  }
}

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
// Captures comparison behavior at the source for any type
struct TriggerInfo {
  std::function<bool()> check_triggered;  // Returns true if trigger fired
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
