#pragma once

#include <coroutine>
#include <exception>
#include <utility>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

class RuntimeProcess;

class ProcessCoroutine {
 public:
  // C++20 coroutine protocol names are spelled by the standard.
  // NOLINTNEXTLINE(readability-identifier-naming)
  struct promise_type {
    // Captures any exception that escapes the coroutine body. `Resume`
    // rethrows it so the original `what()` and type reach the engine
    // unchanged, instead of being collapsed into a generic "coroutine
    // failed" message.
    std::exception_ptr pending_exception;
    // Back-pointer set by RuntimeProcess construction so awaitables can
    // reach the owning RuntimeProcess from `await_suspend` (e.g. to
    // subscribe it to a named event's waiter list, or to look up the
    // current simulation time via the engine's RuntimeServices).
    RuntimeProcess* process = nullptr;

    // NOLINTNEXTLINE(readability-identifier-naming)
    auto get_return_object() -> ProcessCoroutine {
      return ProcessCoroutine{
          std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    // NOLINTNEXTLINE(readability-identifier-naming)
    static auto initial_suspend() noexcept -> std::suspend_always {
      return {};
    }
    // NOLINTNEXTLINE(readability-identifier-naming)
    static auto final_suspend() noexcept -> std::suspend_always {
      return {};
    }
    // NOLINTNEXTLINE(readability-identifier-naming)
    static void return_void() {
    }
    // NOLINTNEXTLINE(readability-identifier-naming)
    void unhandled_exception() noexcept {
      pending_exception = std::current_exception();
    }

    [[nodiscard]] auto Process() const -> RuntimeProcess& {
      if (process == nullptr) {
        throw InternalError(
            "ProcessCoroutine::promise_type::Process: no RuntimeProcess "
            "back-pointer set");
      }
      return *process;
    }
  };

  ProcessCoroutine() = default;
  ProcessCoroutine(const ProcessCoroutine&) = delete;
  auto operator=(const ProcessCoroutine&) -> ProcessCoroutine& = delete;

  ProcessCoroutine(ProcessCoroutine&& other) noexcept
      : handle_(std::exchange(other.handle_, {})) {
  }
  auto operator=(ProcessCoroutine&& other) noexcept -> ProcessCoroutine& {
    if (handle_) {
      handle_.destroy();
    }
    handle_ = std::exchange(other.handle_, {});
    return *this;
  }
  ~ProcessCoroutine() {
    if (handle_) {
      handle_.destroy();
    }
  }

  // Resumes the coroutine. Returns true if the coroutine ran to completion
  // (`co_return` reached); false if it suspended on some awaitable. The
  // awaitable is responsible for arranging its own future wakeup during
  // `await_suspend` -- the engine observes only completion / suspension.
  auto Resume() -> bool {
    if (!handle_ || handle_.done()) {
      return true;
    }
    handle_.resume();
    if (auto exc =
            std::exchange(handle_.promise().pending_exception, nullptr)) {
      std::rethrow_exception(exc);
    }
    return handle_.done();
  }

  [[nodiscard]] auto Done() const -> bool {
    return !handle_ || handle_.done();
  }

  // Wires the promise's back-pointer to the owning RuntimeProcess. Called
  // from RuntimeProcess construction.
  void BindProcess(RuntimeProcess& process) {
    if (handle_) {
      handle_.promise().process = &process;
    }
  }

 private:
  explicit ProcessCoroutine(std::coroutine_handle<promise_type> h)
      : handle_(h) {
  }

  std::coroutine_handle<promise_type> handle_;
};

}  // namespace lyra::runtime
