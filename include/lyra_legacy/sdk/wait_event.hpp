#pragma once

#include <coroutine>
#include <cstdint>
#include <functional>
#include <tuple>

namespace lyra::sdk {

class Scheduler;

// Forward declaration - defined in delay.hpp
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
extern thread_local Scheduler* current_scheduler;

// Edge kind for wait triggers (matches common::EdgeKind)
enum class EdgeKind { kAnyChange, kPosedge, kNegedge, kBothEdge };

namespace detail {

// Concept: type can be read as an integral value for edge detection.
// Uses explicit static_cast (Bit<> has explicit operator T, not implicit).
template <typename T>
concept IntegralReadable = requires(T v) { static_cast<uint64_t>(v); };

// Creates a stateful lambda that checks if a trigger condition is met.
// For edge triggers, requires IntegralReadable<T> (enforced by
// Posedge/Negedge).
template <typename T>
auto MakeTriggerChecker(T* var, EdgeKind kind) -> std::function<bool()> {
  if (kind == EdgeKind::kAnyChange) {
    // Any-change works for any comparable type (strings, structs, etc.)
    return [var, prev = *var]() mutable {
      bool triggered = *var != prev;
      prev = *var;
      return triggered;
    };
  }
  // Edge triggers require reading the value as an integer to detect 0->1/1->0.
  // IntegralReadable<T> is enforced by the Posedge/Negedge factory functions.
  // Use if constexpr to prevent compilation of this branch for non-integral
  // types.
  if constexpr (IntegralReadable<T>) {
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
    // Unreachable at runtime: Posedge/Negedge factory functions have requires
    // clauses that prevent non-IntegralReadable types. This branch exists only
    // for template instantiation when Change() is used with non-integral types.
    // Return a dummy to satisfy the compiler; it will never be called.
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

// Factory functions for creating triggers.
// Posedge/Negedge require IntegralReadable types for edge detection.
template <typename T>
  requires detail::IntegralReadable<T>
auto Posedge(T* var) -> Trigger<T> {
  return Trigger<T>(var, EdgeKind::kPosedge);
}

template <typename T>
  requires detail::IntegralReadable<T>
auto Negedge(T* var) -> Trigger<T> {
  return Trigger<T>(var, EdgeKind::kNegedge);
}

// Change() works with any comparable type (strings, structs, etc.)
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
