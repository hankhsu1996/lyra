#pragma once

#include <coroutine>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// `$finish(level)` / implicit shutdown from `$fatal` -- requests the runtime
// to tear down the simulation after the current slot completes. The awaitable
// also suspends the calling process; since `finished_` is set before
// await_suspend returns, the runtime drops it on the next dispatch. `level`
// arrives as a Lyra value, the same as any other call argument (LRM 20.2).
// `fatal` true (LRM 20.10) marks the termination so the runtime returns a
// non-zero exit code.
class FinishAwaitable {
 public:
  FinishAwaitable(
      RuntimeEffects& runtime, const lyra::value::PackedArray& level,
      bool fatal)
      : runtime_(&runtime),
        level_(static_cast<int>(level.ToInt64())),
        fatal_(fatal) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  // The coroutine protocol passes the awaiting handle, but the finish-family
  // suspends forever (the runtime drops the frame), so the handle is unused.
  // NOLINTNEXTLINE(readability-named-parameter)
  void await_suspend(std::coroutine_handle<>) noexcept {
    runtime_->RequestFinish(level_, fatal_);
  }

  static void await_resume() noexcept {
  }

 private:
  RuntimeEffects* runtime_;
  int level_;
  bool fatal_;
};

inline auto Finish(
    RuntimeEffects& runtime, const lyra::value::PackedArray& level)
    -> FinishAwaitable {
  return FinishAwaitable{runtime, level, false};
}

inline auto FatalFinish(
    RuntimeEffects& runtime, const lyra::value::PackedArray& level)
    -> FinishAwaitable {
  return FinishAwaitable{runtime, level, true};
}

}  // namespace lyra::runtime
