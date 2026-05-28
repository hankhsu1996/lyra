#pragma once

#include <algorithm>
#include <concepts>
#include <coroutine>
#include <cstdint>
#include <initializer_list>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/packed.hpp"

namespace lyra::runtime {

class RuntimeProcess;

// Classification of a single value change by its LSB transition per LRM
// 9.4.2 Table 9-2:
//   - posedge: 0 -> {1, x, z}; {x, z} -> 1
//   - negedge: 1 -> {0, x, z}; {x, z} -> 0
//   - kChangeOnly: any other LSB-different change (x <-> z), or the full
//     value moved but the LSB stayed the same (upper bits changing).
enum class EdgeTransition : std::uint8_t {
  kChangeOnly,
  kPosedge,
  kNegedge,
};

inline auto ClassifyEdge(
    value::FourStateBit old_lsb, value::FourStateBit new_lsb)
    -> EdgeTransition {
  if (old_lsb == new_lsb) {
    return EdgeTransition::kChangeOnly;
  }
  // Leaving 0 is posedge regardless of destination (1, x, or z).
  if (old_lsb == value::FourStateBit::kZero) {
    return EdgeTransition::kPosedge;
  }
  // Leaving 1 is negedge regardless of destination (0, x, or z).
  if (old_lsb == value::FourStateBit::kOne) {
    return EdgeTransition::kNegedge;
  }
  // Leaving x or z: only arrival at 0 or 1 counts; x <-> z is kChangeOnly.
  if (new_lsb == value::FourStateBit::kOne) {
    return EdgeTransition::kPosedge;
  }
  if (new_lsb == value::FourStateBit::kZero) {
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

// Suspends the calling process until one of the supplied Triggers fires
// (matching its edge). Subscribes the process to every Observable directly
// in `await_suspend`; the engine has no idea what kind of wait this is.
// When one Observable wakes the process, the engine sweeps the rest via
// `TakePendingValueChangeSubscriptions` to drop dangling subscriptions.
//
// An empty trigger list is legal -- the process suspends forever (used by
// `always_comb` / `always_latch` with no inferred reads,
// e.g. `always_comb c = 7;`).
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
    auto& process = handle.promise().Process();
    std::vector<Observable*> subs;
    subs.reserve(triggers_.size());
    for (const auto& trigger : triggers_) {
      if (trigger.observable == nullptr) {
        throw InternalError(
            "EventControlAwaitable::await_suspend: observable pointer is "
            "null");
      }
      trigger.observable->Subscribe(process, trigger.edge);
      subs.push_back(trigger.observable);
    }
    process.SetPendingValueChangeSubscriptions(std::move(subs));
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
