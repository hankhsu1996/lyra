#pragma once

#include <concepts>
#include <coroutine>
#include <utility>
#include <vector>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

class RuntimeProcess;

class Observable {
 public:
  Observable() = default;
  Observable(const Observable&) = delete;
  auto operator=(const Observable&) -> Observable& = delete;
  Observable(Observable&&) = delete;
  auto operator=(Observable&&) -> Observable& = delete;
  ~Observable() = default;

  void Subscribe(RuntimeProcess& p) {
    waiters_.push_back(&p);
  }

  [[nodiscard]] auto TakeWaiters() -> std::vector<RuntimeProcess*> {
    return std::exchange(waiters_, {});
  }

 private:
  std::vector<RuntimeProcess*> waiters_;
};

template <typename T>
concept CaseEqualComparable = requires(const T& a, const T& b) {
  { a.IsCaseEqual(b) } -> std::same_as<bool>;
};

template <CaseEqualComparable T>
class Var : public Observable {
 public:
  Var() = default;

  template <typename... Args>
  explicit Var(Args&&... args) : value_(std::forward<Args>(args)...) {
  }

  Var(const Var&) = delete;
  auto operator=(const Var&) -> Var& = delete;
  Var(Var&&) = delete;
  auto operator=(Var&&) -> Var& = delete;
  ~Var() = default;

  // Returns true iff `v` is bit-pattern-different (LRM 9.4.2 `===` semantics
  // for @() detection, not 4-state `==`); caller dispatches on the result.
  auto AssignIfChanged(const T& v) -> bool {
    if (value_.IsCaseEqual(v)) return false;
    value_ = v;
    return true;
  }

  auto AssignIfChanged(T&& v) -> bool {
    if (value_.IsCaseEqual(v)) return false;
    value_ = std::move(v);
    return true;
  }

  // NOLINTNEXTLINE(google-explicit-constructor,hicpp-explicit-conversions)
  operator const T&() const noexcept {
    return value_;
  }

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return value_;
  }

 private:
  T value_{};
};

class ValueChangeAwaitable {
 public:
  explicit ValueChangeAwaitable(Observable& observable)
      : observable_(&observable) {
  }

  // NOLINTNEXTLINE(readability-identifier-naming,readability-convert-member-functions-to-static)
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return false;
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_suspend(
      std::coroutine_handle<ProcessCoroutine::promise_type> handle) noexcept {
    handle.promise().SetWaitRequest(ValueChangeWait{.observable = observable_});
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_resume() const noexcept {
  }

 private:
  Observable* observable_;
};

template <typename T>
auto WaitChange(Var<T>& var) -> ValueChangeAwaitable {
  return ValueChangeAwaitable{var};
}

template <typename T>
void WriteVar(RuntimeServices& services, Var<T>& var, T new_val) {
  if (var.AssignIfChanged(std::move(new_val))) {
    services.TriggerValueChange(var);
  }
}

}  // namespace lyra::runtime
