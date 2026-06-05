#pragma once

#include <coroutine>
#include <exception>
#include <functional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

class Observable;
class RuntimeProcess;

// The single coroutine type for every suspendable activation: a process body
// (`initial` / `always`) and a task subroutine body both compile to it. A
// function body is a plain C++ function and never becomes a Coroutine.
//
// A Coroutine is lazy (suspends before its first statement) and is its own
// awaiter, so a task is enabled with `co_await drive(args)`. When a task
// finishes it transfers control back to its enabling coroutine via the
// continuation stored in the promise (symmetric transfer); a top-level process
// has no continuation and simply suspends at completion for the engine to
// observe.
class Coroutine {
 public:
  struct promise_type {
    // Captures any exception that escapes the coroutine body so the original
    // `what()` and type reach the engine unchanged. A task's exception is
    // rethrown into its enabler at `await_resume`, so it propagates up to the
    // top-level promise where the engine reads it after resuming.
    std::exception_ptr pending_exception;
    // Set on the top-level promise at RuntimeProcess construction and inherited
    // by each enabled task in `await_suspend`, so any frame can recover the
    // owning RuntimeProcess identity from within an awaitable.
    RuntimeProcess* process = nullptr;
    // The coroutine to resume when this one completes; empty for a top-level
    // process. A task records its enabler here when it is co_awaited.
    std::coroutine_handle<promise_type> continuation;
    // Observables this frame subscribed to for its current multi-trigger wait
    // (`@(a or b)`); when one fires the engine sweeps the rest to drop the
    // stale subscriptions. Per-wait state, so it lives on the frame.
    std::vector<Observable*> pending_value_change_subscriptions;
    // Run when this frame completes, before transferring to any continuation. A
    // spawned fork branch sets this to report completion to its join group; the
    // callback owns whatever state it needs (a shared_ptr keeping that group
    // alive), so the coroutine machinery never names the construct.
    std::function<void()> on_complete;

    auto get_return_object() -> Coroutine {
      return Coroutine{
          std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    static auto initial_suspend() noexcept -> std::suspend_always {
      return {};
    }

    // On completion, transfer to the enabler if there is one (a task returning
    // to its caller); otherwise suspend so the engine sees `done()` (a
    // top-level process).
    struct FinalAwaiter {
      [[nodiscard]] static auto await_ready() noexcept -> bool {
        return false;
      }
      static auto await_suspend(
          std::coroutine_handle<promise_type> handle) noexcept
          -> std::coroutine_handle<> {
        if (auto& on_complete = handle.promise().on_complete) {
          on_complete();
        }
        std::coroutine_handle<promise_type> cont =
            handle.promise().continuation;
        if (cont) {
          return cont;
        }
        return std::noop_coroutine();
      }
      static void await_resume() noexcept {
      }
    };
    static auto final_suspend() noexcept -> FinalAwaiter {
      return {};
    }
    static void return_void() {
    }
    void unhandled_exception() noexcept {
      pending_exception = std::current_exception();
    }

    [[nodiscard]] auto Process() const -> RuntimeProcess& {
      if (process == nullptr) {
        throw InternalError(
            "Coroutine::promise_type::Process: no RuntimeProcess back-pointer "
            "set");
      }
      return *process;
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

  // Awaiter surface: `co_await drive(args)` enables a task. Starting the task
  // is a symmetric transfer to its handle; the task carries this enabler as its
  // continuation and inherits its RuntimeProcess identity.
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return !handle_ || handle_.done();
  }
  auto await_suspend(std::coroutine_handle<promise_type> caller) noexcept
      -> std::coroutine_handle<promise_type> {
    handle_.promise().continuation = caller;
    handle_.promise().process = caller.promise().process;
    return handle_;
  }
  void await_resume() const {
    if (auto exc =
            std::exchange(handle_.promise().pending_exception, nullptr)) {
      std::rethrow_exception(exc);
    }
  }

  [[nodiscard]] auto Handle() const -> std::coroutine_handle<promise_type> {
    return handle_;
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
  explicit Coroutine(std::coroutine_handle<promise_type> h) : handle_(h) {
  }

  std::coroutine_handle<promise_type> handle_;
};

// The wait / wakeup token throughout the runtime: the specific coroutine frame
// that suspended (the innermost one when tasks are nested), which is what the
// engine resumes and what awaitables register for wakeup.
using CoroutineHandle = std::coroutine_handle<Coroutine::promise_type>;

}  // namespace lyra::runtime
