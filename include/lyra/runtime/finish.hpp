#pragma once

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// `$finish(level)` -- requests the engine to tear down the simulation after
// the current slot completes. The awaitable also suspends the calling
// process; since `finished_` is set before await_suspend returns, the
// engine drops it on the next dispatch. `level` arrives as a Lyra value, the
// same as any other call argument (LRM 20.2).
class FinishAwaitable {
 public:
  FinishAwaitable(
      RuntimeServices& services, const lyra::value::PackedArray& level)
      : services_(&services), level_(static_cast<int>(level.ToInt64())) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  // The coroutine protocol passes the awaiting handle, but `$finish` suspends
  // forever (the engine drops the frame), so the handle is unused.
  // NOLINTNEXTLINE(readability-named-parameter)
  void await_suspend(CoroutineHandle) noexcept {
    services_->RequestFinish(level_);
  }

  static void await_resume() noexcept {
  }

 private:
  RuntimeServices* services_;
  int level_;
};

inline auto Finish(
    RuntimeServices& services, const lyra::value::PackedArray& level)
    -> FinishAwaitable {
  return FinishAwaitable{services, level};
}

}  // namespace lyra::runtime
