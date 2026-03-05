#pragma once

#include <csetjmp>
#include <cstdint>

namespace lyra::runtime {

enum class TrapReason : uint32_t {
  kLoopBudgetExceeded,
  kUserFatal,
  kInternalError,
};

struct TrapPayload {
  TrapReason reason = TrapReason::kLoopBudgetExceeded;
  uint32_t a = 0;  // e.g., loop_site_id
  uint32_t b = 0;  // spare
};

struct TrapFrame {
  std::jmp_buf env{};
  TrapPayload payload{};
  bool armed = false;
};

// Get the thread-local trap frame pointer.
// Returns nullptr if no trap scope is active.
auto GetTlsTrapFrame() -> TrapFrame*;

// Set the thread-local trap frame pointer.
void SetTlsTrapFrame(TrapFrame* frame);

}  // namespace lyra::runtime

extern "C" {

// Called from generated code to unwind back to the engine scheduler.
// engine_ptr is unused in the current implementation (trap frame is TLS).
[[noreturn]] void LyraTrap(
    void* engine_ptr, uint32_t reason, uint32_t a, uint32_t b);
}
