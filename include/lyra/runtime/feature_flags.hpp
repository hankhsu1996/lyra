#pragma once

#include <cstdint>

namespace lyra::runtime {

// Bitfield flags for optional simulation features.
// Passed as a single uint32_t through the LyraRunSimulation ABI,
// replacing individual boolean parameters for ABI stability.
enum class FeatureFlag : uint32_t {
  kNone = 0,
  // Dump slot metadata registry (test-only)
  kDumpSlotMeta = 1 << 0,
  // Enable simulation tracing
  kEnableTrace = 1 << 1,
  // Enable $system shell command execution
  kEnableSystem = 1 << 2,
  // Dump process metadata registry (test-only)
  kDumpProcessMeta = 1 << 3,
  // Print runtime stats (core counters) after simulation
  kDumpRuntimeStats = 1 << 5,
  // Enable activation trace ring buffer + live output
  kEnableActivationTrace = 1 << 6,
  // Collect detailed per-element counters (subscription, connection, etc.)
  kDetailedStats = 1 << 7,
  // Enable trace summary output (--trace-summary)
  kEnableTraceSummary = 1 << 8,
  // Enable text signal trace output (--trace-signals)
  kEnableSignalTrace = 1 << 9,
};

constexpr auto operator|(FeatureFlag a, FeatureFlag b) -> FeatureFlag {
  return static_cast<FeatureFlag>(
      static_cast<uint32_t>(a) | static_cast<uint32_t>(b));
}

constexpr auto operator&(FeatureFlag a, FeatureFlag b) -> FeatureFlag {
  return static_cast<FeatureFlag>(
      static_cast<uint32_t>(a) & static_cast<uint32_t>(b));
}

constexpr auto HasFlag(FeatureFlag flags, FeatureFlag flag) -> bool {
  return (static_cast<uint32_t>(flags) & static_cast<uint32_t>(flag)) != 0;
}

constexpr auto ToUint32(FeatureFlag f) -> uint32_t {
  return static_cast<uint32_t>(f);
}

}  // namespace lyra::runtime
