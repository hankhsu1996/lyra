#pragma once

#include <cstdint>

namespace lyra::mir {

enum class BuiltinMethodKind : std::uint8_t {
  kEnumFirst,
  kEnumLast,
  kEnumNum,
  kEnumNext,
  kEnumPrev,
  kEnumName,
  // LRM 15.5 named event operations. Trigger is non-suspending; Await suspends
  // the caller until the event fires; Triggered is a pure read of the
  // slot-persistent triggered state.
  kNamedEventTrigger,
  kNamedEventAwait,
  kNamedEventTriggered,
};

// True for methods whose backend emission must wrap the call site in
// `co_await`. The runtime returns an awaitable that submits the appropriate
// WaitRequest via the coroutine's promise.
[[nodiscard]] inline auto IsBuiltinMethodSuspending(BuiltinMethodKind kind)
    -> bool {
  return kind == BuiltinMethodKind::kNamedEventAwait;
}

}  // namespace lyra::mir
