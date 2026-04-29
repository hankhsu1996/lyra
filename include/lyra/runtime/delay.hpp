#pragma once

#include <coroutine>

#include "lyra/base/time.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

class DelayAwaitable {
 public:
  explicit DelayAwaitable(SimDuration duration) : duration_(duration) {
  }

  // NOLINTNEXTLINE(readability-identifier-naming,readability-convert-member-functions-to-static)
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return false;
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_suspend(
      std::coroutine_handle<ProcessCoroutine::promise_type> handle) noexcept {
    handle.promise().SetWaitRequest(DelayRequest{.duration = duration_});
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_resume() const noexcept {
  }

 private:
  SimDuration duration_;
};

inline auto Delay(SimDuration duration) -> DelayAwaitable {
  return DelayAwaitable{duration};
}

}  // namespace lyra::runtime
