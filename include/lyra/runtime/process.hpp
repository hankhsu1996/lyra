#pragma once

#include <coroutine>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

class ProcessCoroutine {
 public:
  // C++20 coroutine protocol names are spelled by the standard.
  // NOLINTNEXTLINE(readability-identifier-naming)
  struct promise_type {
    bool failed = false;
    std::optional<WaitRequest> wait_request;

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
      failed = true;
    }

    void SetWaitRequest(WaitRequest req) {
      wait_request = std::move(req);
    }
    auto TakeWaitRequest() -> std::optional<WaitRequest> {
      auto out = std::move(wait_request);
      wait_request.reset();
      return out;
    }
    void ClearWaitRequest() {
      wait_request.reset();
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

  auto Resume() -> ProcessRunResult {
    if (!handle_ || handle_.done()) {
      return ProcessRunResult::Completed();
    }
    handle_.promise().ClearWaitRequest();
    handle_.resume();
    if (handle_.promise().failed) {
      handle_.promise().failed = false;
      throw InternalError(
          "lyra::runtime::ProcessCoroutine::Resume: process coroutine failed");
    }
    if (handle_.done()) {
      return ProcessRunResult::Completed();
    }
    auto wait = handle_.promise().TakeWaitRequest();
    if (!wait.has_value()) {
      throw InternalError(
          "lyra::runtime::ProcessCoroutine::Resume: process suspended without "
          "a wait request");
    }
    return ProcessRunResult::Suspended(std::move(*wait));
  }

  [[nodiscard]] auto Done() const -> bool {
    return !handle_ || handle_.done();
  }

 private:
  explicit ProcessCoroutine(std::coroutine_handle<promise_type> h)
      : handle_(h) {
  }

  std::coroutine_handle<promise_type> handle_;
};

}  // namespace lyra::runtime
