#include "lyra/runtime/trap.hpp"

#include <csetjmp>
#include <cstdint>
#include <cstdio>
#include <cstdlib>

namespace lyra::runtime {

namespace {

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
thread_local TrapFrame* tls_trap_frame = nullptr;

}  // namespace

auto GetTlsTrapFrame() -> TrapFrame* {
  return tls_trap_frame;
}

void SetTlsTrapFrame(TrapFrame* frame) {
  tls_trap_frame = frame;
}

}  // namespace lyra::runtime

extern "C" [[noreturn]] void LyraTrap(
    void* /*engine_ptr*/, uint32_t reason, uint32_t a, uint32_t b) {
  auto* frame = lyra::runtime::GetTlsTrapFrame();
  if (frame == nullptr || !frame->armed) {
    // No trap scope active - fatal abort (should never happen in normal flow)
    fprintf(stderr, "LyraTrap called without active trap scope\n");  // NOLINT
    std::abort();
  }

  frame->payload = lyra::runtime::TrapPayload{
      .reason = static_cast<lyra::runtime::TrapReason>(reason),
      .a = a,
      .b = b,
  };
  frame->armed = false;

  // NOLINTNEXTLINE(cert-err52-cpp)
  std::longjmp(frame->env, 1);
}
