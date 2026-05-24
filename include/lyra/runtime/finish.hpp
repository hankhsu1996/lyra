#pragma once

#include <coroutine>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

class FinishAwaitable {
 public:
  explicit FinishAwaitable(int level) : level_(level) {
  }

  // NOLINTNEXTLINE(readability-identifier-naming,readability-convert-member-functions-to-static)
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return false;
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_suspend(
      std::coroutine_handle<ProcessCoroutine::promise_type> handle) noexcept {
    handle.promise().SetWaitRequest(FinishWait{.level = level_});
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_resume() const noexcept {
  }

 private:
  int level_;
};

inline auto Finish(int level) -> FinishAwaitable {
  return FinishAwaitable{level};
}

}  // namespace lyra::runtime
