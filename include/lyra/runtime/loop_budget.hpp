#pragma once

#include <cstdint>

inline constexpr uint32_t kDefaultLoopBudget = 10'000'000;

extern "C" {

// Returns pointer to TLS loop iteration counter.
// Generated code decrements this; when it reaches 0, calls LyraTrap.
auto LyraLoopBudgetPtr() -> uint32_t*;

// Reset TLS loop budget to the given value.
// Called by engine before each runner_() invocation.
void LyraResetLoopBudget(uint32_t value);
}
