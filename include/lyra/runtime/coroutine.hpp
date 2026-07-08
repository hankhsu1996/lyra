#pragma once

#include <coroutine>
#include <exception>
#include <functional>
#include <optional>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

class Observable;
class RuntimeProcess;

// Non-templated scheduling state shared by every coroutine frame -- a process
// body, a task body, a fork branch -- regardless of the value the frame
// completes with. The engine and the awaitables hold a `PromiseBase*` as the
// universal wakeup token and read scheduling state through it, never naming the
// frame's result type. `self` is this frame's own handle, type-erased, so the
// engine resumes and queries completion without knowing the result type
// (recovering a promise from a frame address is not a portable ABI, so the
// handle is stored at construction, not derived).
struct PromiseBase {
  std::exception_ptr pending_exception;
  RuntimeProcess* process = nullptr;
  std::coroutine_handle<> continuation;
  std::vector<Observable*> pending_value_change_subscriptions;
  std::function<void()> on_complete;
  std::coroutine_handle<> self;

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

  void unhandled_exception() noexcept {
    pending_exception = std::current_exception();
  }

  [[nodiscard]] auto Process() const -> RuntimeProcess& {
    if (process == nullptr) {
      throw InternalError(
          "PromiseBase::Process: no RuntimeProcess back-pointer set");
    }
    return *process;
  }
};

// The universal wakeup / scheduling token: a pointer to a suspended frame's
// scheduling state. The engine queues it, awaitables register it, and resuming
// a frame goes through `token->self`.
using CoroutineHandle = PromiseBase*;

// The completion-value storage split, kept off `PromiseBase` so the scheduler
// never sees `T`: a value-completing frame keeps the produced value until the
// awaiter moves it out; a void-completing frame keeps nothing. `std::optional`
// because `T` need not be default-constructible and "not yet produced" (e.g. an
// exceptional exit) is a real state.
template <class T>
struct CoroutineResult {
  std::optional<T> result;
  void return_value(T value) {
    result = std::move(value);
  }
};

template <>
struct CoroutineResult<void> {
  void return_void() {
  }
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
  struct promise_type : PromiseBase, CoroutineResult<T> {
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
    if (auto exc =
            std::exchange(handle_.promise().pending_exception, nullptr)) {
      std::rethrow_exception(exc);
    }
    if constexpr (!std::is_void_v<T>) {
      return std::move(*handle_.promise().result);
    }
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
