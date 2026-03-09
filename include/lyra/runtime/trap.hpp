#pragma once

#include <cstdint>
#include <optional>

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

// Consume the pending TLS trap payload (exactly-once delivery).
// Returns the payload and clears the pending flag.
// Returns nullopt if no trap is pending.
auto ConsumeTlsTrap() -> std::optional<TrapPayload>;

// Hard-reset TLS trap state at activation entry.
// Guarantees every activation starts clean.
void ResetTlsTrap();

}  // namespace lyra::runtime

extern "C" {

// Called from generated code to capture a trap payload into TLS and return.
// The process function must then return kTrap to its caller.
// Internal error if called while a payload is already pending.
void LyraTrap(void* engine_ptr, uint32_t reason, uint32_t a, uint32_t b);
}
