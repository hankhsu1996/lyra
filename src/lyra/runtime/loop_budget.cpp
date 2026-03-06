#include "lyra/runtime/loop_budget.hpp"

#include <cstdint>

namespace {

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
thread_local uint32_t tls_loop_budget = kDefaultLoopBudget;

}  // namespace

extern "C" auto LyraLoopBudgetPtr() -> uint32_t* {
  return &tls_loop_budget;
}

extern "C" void LyraResetLoopBudget(uint32_t value) {
  tls_loop_budget = value;
}
