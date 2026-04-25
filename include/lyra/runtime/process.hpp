#pragma once

#include <coroutine>
#include <utility>

#include "lyra/support/internal_error.hpp"

namespace lyra::runtime {

class Process {
 public:
  // C++20 coroutine protocol names are spelled by the standard.
  // NOLINTNEXTLINE(readability-identifier-naming)
  struct promise_type {
    bool failed = false;

    // NOLINTNEXTLINE(readability-identifier-naming)
    auto get_return_object() -> Process {
      return Process{std::coroutine_handle<promise_type>::from_promise(*this)};
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
  };

  Process() = default;
  Process(const Process&) = delete;
  auto operator=(const Process&) -> Process& = delete;

  Process(Process&& other) noexcept
      : handle_(std::exchange(other.handle_, {})) {
  }
  auto operator=(Process&& other) noexcept -> Process& {
    if (handle_) {
      handle_.destroy();
    }
    handle_ = std::exchange(other.handle_, {});
    return *this;
  }
  ~Process() {
    if (handle_) {
      handle_.destroy();
    }
  }

  void Resume() {
    if (!handle_) {
      throw support::InternalError(
          "lyra::runtime::Process::Resume on empty handle");
    }
    if (handle_.done()) {
      throw support::InternalError(
          "lyra::runtime::Process::Resume on already-done coroutine");
    }
    handle_.resume();
    if (handle_.promise().failed) {
      handle_.promise().failed = false;
      throw support::InternalError("lyra::runtime::Process body failed");
    }
  }

  [[nodiscard]] auto Done() const -> bool {
    return !handle_ || handle_.done();
  }

 private:
  explicit Process(std::coroutine_handle<promise_type> h) : handle_(h) {
  }

  std::coroutine_handle<promise_type> handle_;
};

}  // namespace lyra::runtime
