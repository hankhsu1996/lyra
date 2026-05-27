#pragma once

#include <algorithm>
#include <concepts>
#include <coroutine>
#include <cstdint>
#include <initializer_list>
#include <utility>
#include <vector>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/wait_request.hpp"
#include "lyra/value/packed.hpp"

namespace lyra::runtime {

class RuntimeProcess;

// Classification of a single value change by its LSB transition (LRM 9.4.2
// Table 9-2). `kChangeOnly` covers transitions that move the full value but
// don't cross 0 or 1 on the LSB (e.g. x->z, or upper bits changing while the
// LSB stays the same).
enum class EdgeTransition : std::uint8_t {
  kChangeOnly,
  kPosedge,
  kNegedge,
};

inline auto ClassifyEdge(
    value::FourStateBit old_lsb, value::FourStateBit new_lsb)
    -> EdgeTransition {
  if (new_lsb == value::FourStateBit::kOne &&
      old_lsb != value::FourStateBit::kOne) {
    return EdgeTransition::kPosedge;
  }
  if (new_lsb == value::FourStateBit::kZero &&
      old_lsb != value::FourStateBit::kZero) {
    return EdgeTransition::kNegedge;
  }
  return EdgeTransition::kChangeOnly;
}

inline auto EdgeMatches(Edge subscribed, EdgeTransition transition) -> bool {
  switch (subscribed) {
    case Edge::kAnyChange:
      return true;
    case Edge::kPosedge:
      return transition == EdgeTransition::kPosedge;
    case Edge::kNegedge:
      return transition == EdgeTransition::kNegedge;
    case Edge::kBothEdges:
      return transition == EdgeTransition::kPosedge ||
             transition == EdgeTransition::kNegedge;
  }
  return false;
}

class Observable {
 public:
  Observable() = default;
  Observable(const Observable&) = delete;
  auto operator=(const Observable&) -> Observable& = delete;
  Observable(Observable&&) = delete;
  auto operator=(Observable&&) -> Observable& = delete;
  ~Observable() = default;

  void Subscribe(RuntimeProcess& p, Edge edge) {
    waiters_.push_back(Waiter{.process = &p, .edge = edge});
  }

  // Idempotent: a no-op if `p` is not currently subscribed. Used to clean up
  // sibling subscriptions when a multi-trigger wait resumes on one Observable.
  void Unsubscribe(RuntimeProcess& p) {
    std::erase_if(waiters_, [&](const Waiter& w) { return w.process == &p; });
  }

  // Removes and returns the processes whose subscribed Edge matches the
  // transition that just occurred. Non-matching waiters stay subscribed.
  [[nodiscard]] auto TakeMatchingWaiters(EdgeTransition transition)
      -> std::vector<RuntimeProcess*> {
    std::vector<RuntimeProcess*> out;
    auto keep_begin = std::ranges::partition(waiters_, [&](const Waiter& w) {
                        return !EdgeMatches(w.edge, transition);
                      }).begin();
    out.reserve(static_cast<std::size_t>(waiters_.end() - keep_begin));
    for (auto it = keep_begin; it != waiters_.end(); ++it) {
      out.push_back(it->process);
    }
    waiters_.erase(keep_begin, waiters_.end());
    return out;
  }

 private:
  struct Waiter {
    RuntimeProcess* process;
    Edge edge;
  };
  std::vector<Waiter> waiters_;
};

template <typename T>
concept CaseEqualComparable = requires(const T& a, const T& b) {
  { a.IsCaseEqual(b) } -> std::same_as<bool>;
};

template <typename T>
concept HasLsb = requires(const T& a) {
  { a.Lsb() } -> std::same_as<value::FourStateBit>;
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

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return value_;
  }

 private:
  T value_{};
};

class EventControlAwaitable {
 public:
  explicit EventControlAwaitable(std::vector<Trigger> triggers)
      : triggers_(std::move(triggers)) {
  }

  // NOLINTNEXTLINE(readability-identifier-naming,readability-convert-member-functions-to-static)
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return false;
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_suspend(
      std::coroutine_handle<ProcessCoroutine::promise_type> handle) {
    handle.promise().SetWaitRequest(
        ValueChangeWait{.triggers = std::move(triggers_)});
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_resume() const noexcept {
  }

 private:
  std::vector<Trigger> triggers_;
};

inline auto WaitAny(std::initializer_list<Trigger> triggers)
    -> EventControlAwaitable {
  return EventControlAwaitable{std::vector<Trigger>(triggers)};
}

template <HasLsb T>
void WriteVar(RuntimeServices& services, Var<T>& var, T new_val) {
  const value::FourStateBit old_lsb = var.Get().Lsb();
  const value::FourStateBit new_lsb = new_val.Lsb();
  if (var.AssignIfChanged(std::move(new_val))) {
    services.TriggerValueChange(var, ClassifyEdge(old_lsb, new_lsb));
  }
}

}  // namespace lyra::runtime
