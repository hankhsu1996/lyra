#pragma once

#include <coroutine>
#include <cstdint>
#include <functional>
#include <tuple>
#include <type_traits>

namespace lyra::sdk {

class Scheduler;

// Forward declaration - defined in delay.hpp
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
extern thread_local Scheduler* current_scheduler;

// Edge kind for wait triggers (matches common::EdgeKind)
enum class EdgeKind { kAnyChange, kPosedge, kNegedge, kBothEdge };

namespace detail {

// Creates a stateful lambda that checks if a trigger condition is met
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

}  // namespace detail

// NOLINTBEGIN(readability-identifier-naming)
// Coroutine awaitable requires specific naming convention from C++ standard

// Trigger represents a single wait condition on a variable.
// Can be used directly as an awaitable: co_await Posedge(&clk);
template <typename T>
class Trigger {
 public:
  Trigger(T* var, EdgeKind kind) : var_(var), kind_(kind) {
  }

  [[nodiscard]] auto var() const -> T* {
    return var_;
  }
  [[nodiscard]] auto kind() const -> EdgeKind {
    return kind_;
  }

  [[nodiscard]] auto MakeChecker() const -> std::function<bool()> {
    return detail::MakeTriggerChecker(var_, kind_);
  }

  // Awaitable interface
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

// Factory functions for creating triggers
template <typename T>
auto Posedge(T* var) -> Trigger<T> {
  return Trigger<T>(var, EdgeKind::kPosedge);
}

template <typename T>
auto Negedge(T* var) -> Trigger<T> {
  return Trigger<T>(var, EdgeKind::kNegedge);
}

template <typename T>
auto Change(T* var) -> Trigger<T> {
  return Trigger<T>(var, EdgeKind::kAnyChange);
}

// AnyChange awaitable - waits for any of the given variables to change.
// Optimized for always_comb sensitivity lists where all triggers are AnyChange.
// Usage: co_await AnyChange(&a, &b, &c);
template <typename... Ts>
class AnyChangeAwaitable {
 public:
  explicit AnyChangeAwaitable(Ts*... vars) : vars_(vars...) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  // Defined in scheduler.hpp after Scheduler is complete
  void await_suspend(std::coroutine_handle<> handle) const;

  static void await_resume() {
  }

  [[nodiscard]] auto vars() const -> const std::tuple<Ts*...>& {
    return vars_;
  }

 private:
  std::tuple<Ts*...> vars_;
};

template <typename... Ts>
auto AnyChange(Ts*... vars) -> AnyChangeAwaitable<Ts...> {
  return AnyChangeAwaitable<Ts...>(vars...);
}

// AnyOf awaitable - waits for any of the given triggers to fire.
// Supports mixed edge types: co_await AnyOf(Posedge(&clk), Negedge(&rst));
template <typename... Triggers>
class AnyOfAwaitable {
 public:
  explicit AnyOfAwaitable(Triggers... triggers) : triggers_(triggers...) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  // Defined in scheduler.hpp after Scheduler is complete
  void await_suspend(std::coroutine_handle<> handle) const;

  static void await_resume() {
  }

  [[nodiscard]] auto triggers() const -> const std::tuple<Triggers...>& {
    return triggers_;
  }

 private:
  std::tuple<Triggers...> triggers_;
};

template <typename... Triggers>
auto AnyOf(Triggers... triggers) -> AnyOfAwaitable<Triggers...> {
  return AnyOfAwaitable<Triggers...>(triggers...);
}

// NOLINTEND(readability-identifier-naming)

}  // namespace lyra::sdk
