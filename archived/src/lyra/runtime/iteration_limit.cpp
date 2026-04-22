#include "lyra/runtime/iteration_limit.hpp"

#include <cstdint>

namespace {

// Process-global iteration limit, set once at startup.
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
uint32_t g_iteration_limit = kDefaultIterationLimit;

// Per-thread iteration counter, decremented by generated back-edge guards.
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
thread_local uint32_t tls_iteration_limit = kDefaultIterationLimit;

}  // namespace

extern "C" auto LyraIterationLimitPtr() -> uint32_t* {
  return &tls_iteration_limit;
}

extern "C" void LyraResetIterationLimit(uint32_t value) {
  tls_iteration_limit = value;
}

extern "C" void LyraSetIterationLimit(uint32_t value) {
  g_iteration_limit = value;
}

extern "C" auto LyraGetIterationLimit() -> uint32_t {
  return g_iteration_limit;
}
