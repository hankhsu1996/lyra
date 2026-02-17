#pragma once

#include <cstdint>

namespace lyra::runtime {

// Bitfield flags for optional simulation features.
// Passed as a single uint32_t through the LyraRunSimulation ABI,
// replacing individual boolean parameters for ABI stability.
enum class FeatureFlag : uint32_t {
  kNone = 0,
  kDumpSlotMeta = 1 << 0,  // Dump slot metadata registry (test-only)
  kEnableTrace = 1 << 1,   // Enable simulation tracing
  kEnableSystem = 1 << 2,  // Enable $system shell command execution
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
