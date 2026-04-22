#pragma once

#include <cstdint>

inline constexpr uint32_t kDefaultIterationLimit = 1'000'000'000;

extern "C" {

// Returns pointer to TLS iteration counter.
// Generated code decrements this; when it reaches 0, returns a kTrap outcome.
auto LyraIterationLimitPtr() -> uint32_t*;

// Reset TLS iteration counter to the given value.
// Called by engine before each process activation.
void LyraResetIterationLimit(uint32_t value);

// Set the process-global iteration limit used by LyraRunProcessSync.
// Called once at startup from the CLI or the generated main.
// 0 = unlimited.
void LyraSetIterationLimit(uint32_t value);

// Get the current process-global iteration limit.
auto LyraGetIterationLimit() -> uint32_t;
}
