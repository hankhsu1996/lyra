#pragma once

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// `$finish(level)` / implicit shutdown from `$fatal` -- requests the engine
// to tear down the simulation after the current slot completes. The awaitable
// also suspends the calling process; since `finished_` is set before
// await_suspend returns, the engine drops it on the next dispatch. `level`
// arrives as a Lyra value, the same as any other call argument (LRM 20.2).
// `fatal` true (LRM 20.10) marks the termination so the engine returns a
// non-zero exit code.
class FinishAwaitable {
 public:
  FinishAwaitable(
      RuntimeServices& services, const lyra::value::PackedArray& level,
      bool fatal)
      : services_(&services),
        level_(static_cast<int>(level.ToInt64())),
        fatal_(fatal) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  // The coroutine protocol passes the awaiting handle, but the finish-family
  // suspends forever (the engine drops the frame), so the handle is unused.
  // NOLINTNEXTLINE(readability-named-parameter)
  void await_suspend(CoroutineHandle) noexcept {
    services_->RequestFinish(level_, fatal_);
  }

  static void await_resume() noexcept {
  }

 private:
  RuntimeServices* services_;
  int level_;
  bool fatal_;
};

inline auto Finish(
    RuntimeServices& services, const lyra::value::PackedArray& level)
    -> FinishAwaitable {
  return FinishAwaitable{services, level, false};
}

inline auto FatalFinish(
    RuntimeServices& services, const lyra::value::PackedArray& level)
    -> FinishAwaitable {
  return FinishAwaitable{services, level, true};
}

}  // namespace lyra::runtime
