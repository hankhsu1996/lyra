#pragma once

#include <coroutine>
#include <exception>

namespace lyra::sdk {

// NOLINTBEGIN(readability-identifier-naming)
// Coroutine promise_type requires specific naming convention from C++ standard

class Task {
 public:
  struct promise_type {
    auto get_return_object() -> Task {
      return Task{std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    static auto initial_suspend() -> std::suspend_never {
      return {};
    }
    static auto final_suspend() noexcept -> std::suspend_always {
      return {};
    }
    static void return_void() {
    }
    static void unhandled_exception() {
      std::terminate();
    }
  };

  // NOLINTEND(readability-identifier-naming)

  using Handle = std::coroutine_handle<promise_type>;

  explicit Task(Handle handle) : handle_(handle) {
  }
  ~Task() {
    if (handle_) {
      handle_.destroy();
    }
  }

  Task(const Task&) = delete;
  auto operator=(const Task&) -> Task& = delete;
  Task(Task&& other) noexcept : handle_(other.handle_) {
    other.handle_ = nullptr;
  }
  auto operator=(Task&& other) noexcept -> Task& {
    if (this != &other) {
      if (handle_) {
        handle_.destroy();
      }
      handle_ = other.handle_;
      other.handle_ = nullptr;
    }
    return *this;
  }

  [[nodiscard]] auto Done() const -> bool {
    return handle_.done();
  }
  void Resume() {
    handle_.resume();
  }

 private:
  Handle handle_;
};

}  // namespace lyra::sdk
