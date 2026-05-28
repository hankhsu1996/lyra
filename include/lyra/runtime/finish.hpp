#pragma once

#include <coroutine>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

// `$finish(level)` -- requests the engine to tear down the simulation after
// the current slot completes. The awaitable also suspends the calling
// process; since `finished_` is set before await_suspend returns, the
// engine drops it on the next dispatch.
class FinishAwaitable {
 public:
  FinishAwaitable(RuntimeServices& services, int level)
      : services_(&services), level_(level) {
  }

  // NOLINTNEXTLINE(readability-identifier-naming,readability-convert-member-functions-to-static)
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return false;
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_suspend(
      std::coroutine_handle<ProcessCoroutine::promise_type> handle) noexcept {
    (void)handle;
    services_->RequestFinish(level_);
  }

  // NOLINTNEXTLINE(readability-identifier-naming)
  void await_resume() const noexcept {
  }

 private:
  RuntimeServices* services_;
  int level_;
};

inline auto Finish(RuntimeServices& services, int level) -> FinishAwaitable {
  return FinishAwaitable{services, level};
}

}  // namespace lyra::runtime
