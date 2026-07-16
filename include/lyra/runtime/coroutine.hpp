#pragma once

#include <coroutine>
#include <deque>
#include <exception>
#include <functional>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

class RuntimeProcess;
class PendingWait;

// Non-templated scheduling state shared by every coroutine frame -- a process
// body, a task body, a fork branch -- regardless of the value the frame
// completes with. The engine and the awaitables hold a `PromiseBase*` as the
// universal wakeup token and read scheduling state through it, never naming the
// frame's result type. `self` is this frame's own handle, type-erased, so the
// engine resumes and queries completion without knowing the result type
// (recovering a promise from a frame address is not a portable ABI, so the
// handle is stored at construction, not derived).
struct PromiseBase {
  RuntimeProcess* process = nullptr;
  std::coroutine_handle<> continuation;
  std::function<void()> on_complete;
  std::coroutine_handle<> self;
  // Every membership this activation currently holds: the observables of a
  // value-change wait (`@(a or b)` holds one each), a named event, a join or
  // `wait fork` condition, or the scheduler queue or delay slot it sits in. It
  // owns them, so releasing it unlinks them all and leaves nothing able to
  // resume it. A deque because the targets' lists point at these addresses, and
  // appending must not move the ones already linked.
  std::deque<Registration> registrations;
  // The wait this activation is blocked on, if any: a capability, held by the
  // awaiter that suspended this frame, to re-establish the wait on resume (LRM
  // 9.7). Distinct from `registrations`, which is the current enrollment; a
  // suspend revokes the enrollment but keeps this. Null while runnable,
  // executing, or terminal. The awaiter is frame-resident, so this points
  // within the same frame and dies with it.
  PendingWait* pending_wait = nullptr;

  PromiseBase() = default;
  PromiseBase(const PromiseBase&) = delete;
  auto operator=(const PromiseBase&) -> PromiseBase& = delete;
  PromiseBase(PromiseBase&&) = delete;
  auto operator=(PromiseBase&&) -> PromiseBase& = delete;
  ~PromiseBase() = default;

  // Parks this activation on `target` and hands back the membership, so a
  // caller whose target carries a fire condition can record it.
  auto Park(RegistrationList& target) -> Registration& {
    Registration& reg = registrations.emplace_back();
    reg.activation = this;
    target.PushBack(reg);
    return reg;
  }

  // Drops every membership: the activation is runnable again, so nothing it was
  // parked on may fire it a second time.
  void RevokeRegistrations() noexcept {
    registrations.clear();
  }

  // A promise protocol hook is an instance customization point by contract, so
  // it stays a member even when an implementation reads no promise state; the
  // convert-to-static check is a false positive here.
  // NOLINTNEXTLINE(readability-convert-member-functions-to-static)
  auto initial_suspend() noexcept -> std::suspend_always {
    return {};
  }

  // On completion, run the completion hook (a fork branch reports to its join
  // group) then transfer to the enabler if there is one (a task returning to
  // its caller); a top-level process has no continuation and suspends so the
  // engine sees `done()`.
  struct FinalAwaiter {
    PromiseBase* promise;
    // A coroutine awaiter hook is an instance customization point by contract,
    // so it stays a member even when an implementation reads no awaiter state;
    // the convert-to-static check is a false positive here.
    // NOLINTNEXTLINE(readability-convert-member-functions-to-static)
    [[nodiscard]] auto await_ready() const noexcept -> bool {
      return false;
    }
    // NOLINTNEXTLINE(readability-named-parameter)
    [[nodiscard]] auto await_suspend(std::coroutine_handle<>) const noexcept
        -> std::coroutine_handle<> {
      if (promise->on_complete) {
        promise->on_complete();
      }
      if (promise->continuation) {
        return promise->continuation;
      }
      return std::noop_coroutine();
    }
    // NOLINTNEXTLINE(readability-convert-member-functions-to-static)
    void await_resume() const noexcept {
    }
  };
  auto final_suspend() noexcept -> FinalAwaiter {
    return FinalAwaiter{.promise = this};
  }

  [[nodiscard]] auto Process() const -> RuntimeProcess& {
    if (process == nullptr) {
      throw InternalError(
          "PromiseBase::Process: no RuntimeProcess back-pointer set");
    }
    return *process;
  }
};

// The single typed terminal outcome an activation settles: a produced value or
// an exception. Kept off `PromiseBase` so the scheduler never sees `T` or the
// outcome. The slot stores the outcome and hands it to the activation's one
// consumer; whether a fault is re-raised in place or extracted first and
// re-raised after the frame is torn down is the consumer's decision, not the
// slot's. The alternatives are mutually exclusive -- a frame runs
// `return_value` / `return_void` xor `unhandled_exception` -- and the initial
// monostate is the not-yet-settled state. A cancelled activation settles
// nothing: it is released while parked, so its slot is never read, and the
// monostate is where it ends.
template <class T>
class CompletionSlot {
 public:
  void return_value(T value) {
    outcome_.template emplace<Value>(std::move(value));
  }
  void unhandled_exception() noexcept {
    outcome_.template emplace<std::exception_ptr>(std::current_exception());
  }
  auto Take() -> T {
    if (auto* exc = std::get_if<std::exception_ptr>(&outcome_)) {
      std::rethrow_exception(*exc);
    }
    return std::move(std::get<Value>(outcome_).held);
  }

 private:
  struct Value {
    T held;
  };
  std::variant<std::monostate, Value, std::exception_ptr> outcome_;
};

template <>
class CompletionSlot<void> {
 public:
  void return_void() {
    outcome_.emplace<Succeeded>();
  }
  void unhandled_exception() noexcept {
    outcome_.emplace<std::exception_ptr>(std::current_exception());
  }
  // Hands the fault out without raising it (null when the activation
  // succeeded), so a consumer that must run its own teardown before the fault
  // propagates can settle first and re-raise the returned outcome afterward.
  auto TakeFault() -> std::exception_ptr {
    if (auto* exc = std::get_if<std::exception_ptr>(&outcome_)) {
      return std::move(*exc);
    }
    return nullptr;
  }
  void Take() {
    if (auto fault = TakeFault()) {
      std::rethrow_exception(fault);
    }
  }

 private:
  struct Succeeded {};
  std::variant<std::monostate, Succeeded, std::exception_ptr> outcome_;
};

// A suspendable activation that completes with a value of `T` -- a task body,
// or a process body with `T = void`. The completion value travels through the
// promise: the body's `co_return v` stores it, and the awaiting frame moves it
// out in `await_resume` before this `Coroutine` (which owns the frame) is
// destroyed. All scheduling lives on the non-templated `PromiseBase` the engine
// sees; nothing per-`T` reaches the scheduler.
//
// A `Coroutine` is lazy (suspends before its first statement) and is its own
// awaiter, so a task is enabled with `co_await task(args)`. The awaiting
// frame's receiver is a single consumer of the completion value.
template <class T>
class Coroutine {
 public:
  struct promise_type : PromiseBase, CompletionSlot<T> {
    auto get_return_object() -> Coroutine {
      auto handle = std::coroutine_handle<promise_type>::from_promise(*this);
      self = handle;
      return Coroutine{handle};
    }
  };

  Coroutine() = default;
  Coroutine(const Coroutine&) = delete;
  auto operator=(const Coroutine&) -> Coroutine& = delete;

  Coroutine(Coroutine&& other) noexcept
      : handle_(std::exchange(other.handle_, {})) {
  }
  auto operator=(Coroutine&& other) noexcept -> Coroutine& {
    if (handle_) {
      handle_.destroy();
    }
    handle_ = std::exchange(other.handle_, {});
    return *this;
  }
  ~Coroutine() {
    if (handle_) {
      handle_.destroy();
    }
  }

  // Awaiter surface: `co_await task(args)` enables the task. Starting it is a
  // symmetric transfer to its handle; the task carries this enabler as its
  // continuation and inherits its RuntimeProcess identity. The enabler may be a
  // process or another task, so the promise type is deduced.
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return !handle_ || handle_.done();
  }
  template <class P>
  auto await_suspend(std::coroutine_handle<P> caller) noexcept
      -> std::coroutine_handle<promise_type> {
    handle_.promise().continuation = caller;
    handle_.promise().process = caller.promise().process;
    return handle_;
  }
  auto await_resume() -> T {
    return handle_.promise().Take();
  }

  [[nodiscard]] auto Handle() const -> std::coroutine_handle<promise_type> {
    return handle_;
  }

  // The scheduling token for this frame -- what the engine queues and resumes.
  [[nodiscard]] auto Token() const -> CoroutineHandle {
    return handle_ ? &handle_.promise() : nullptr;
  }

  [[nodiscard]] auto Done() const -> bool {
    return !handle_ || handle_.done();
  }

  // Wires the promise's back-pointer to the owning RuntimeProcess. Called from
  // RuntimeProcess construction for the top-level coroutine.
  void BindProcess(RuntimeProcess& process) {
    if (handle_) {
      handle_.promise().process = &process;
    }
  }

 private:
  explicit Coroutine(std::coroutine_handle<promise_type> handle)
      : handle_(handle) {
  }

  std::coroutine_handle<promise_type> handle_;
};

}  // namespace lyra::runtime
