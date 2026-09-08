#pragma once

#include <coroutine>
#include <string_view>
#include <utility>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// A simulation control task (LRM 20.2) and the implicit `$finish` a `$fatal`
// makes (LRM 20.10) -- ends the run after the current slot completes. The
// awaitable also suspends the calling process; since the ending is recorded
// before await_suspend returns, the runtime drops it on the next dispatch.
// `origin` and `level` arrive as Lyra values, the same as any other call
// argument. `$stop` suspends where `$finish` exits, and a run nothing can
// resume tells the two apart only in what it prints, so the task's own name is
// what this carries.
class SimulationControlAwaitable {
 public:
  SimulationControlAwaitable(
      RuntimeEffects& runtime, std::string_view task,
      lyra::value::String origin, lyra::value::PackedArray level)
      : runtime_(&runtime),
        task_(task),
        origin_(std::move(origin)),
        level_(std::move(level)) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  // The coroutine protocol passes the awaiting handle, but a control task
  // suspends forever (the runtime drops the frame), so the handle is unused.
  // NOLINTNEXTLINE(readability-named-parameter)
  void await_suspend(std::coroutine_handle<>) {
    runtime_->EndRun(task_, origin_, level_);
  }

  static void await_resume() noexcept {
  }

 private:
  RuntimeEffects* runtime_;
  std::string_view task_;
  lyra::value::String origin_;
  lyra::value::PackedArray level_;
};

inline auto Finish(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    const lyra::value::PackedArray& level) -> SimulationControlAwaitable {
  return SimulationControlAwaitable{runtime, "$finish", origin, level};
}

inline auto Stop(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    const lyra::value::PackedArray& level) -> SimulationControlAwaitable {
  return SimulationControlAwaitable{runtime, "$stop", origin, level};
}

}  // namespace lyra::runtime
