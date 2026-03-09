#include "lyra/runtime/trap.hpp"

#include <cstdint>
#include <optional>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

namespace {

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
thread_local TrapPayload tls_trap_payload{};
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
thread_local bool tls_trap_pending = false;

}  // namespace

auto ConsumeTlsTrap() -> std::optional<TrapPayload> {
  if (!tls_trap_pending) {
    return std::nullopt;
  }
  auto payload = tls_trap_payload;
  tls_trap_payload = {};
  tls_trap_pending = false;
  return payload;
}

void ResetTlsTrap() {
  tls_trap_payload = {};
  tls_trap_pending = false;
}

}  // namespace lyra::runtime

extern "C" void LyraTrap(
    void* /*engine_ptr*/, uint32_t reason, uint32_t a, uint32_t b) {
  if (lyra::runtime::tls_trap_pending) {
    throw lyra::common::InternalError(
        "LyraTrap", "trap raised while a payload is already pending");
  }

  lyra::runtime::tls_trap_payload = lyra::runtime::TrapPayload{
      .reason = static_cast<lyra::runtime::TrapReason>(reason),
      .a = a,
      .b = b,
  };
  lyra::runtime::tls_trap_pending = true;
}
