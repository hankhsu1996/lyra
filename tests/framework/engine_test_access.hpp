#pragma once

#include <chrono>
#include <cstdint>

#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

// Test-only access to Engine internals. Friended by Engine.
struct EngineTestAccess {
  static void SetObservabilityConfig(
      Engine& engine, uint32_t clock_check_interval,
      std::chrono::steady_clock::duration stdout_flush_period) {
    engine.observability_config_.clock_check_interval = clock_check_interval;
    engine.observability_config_.stdout_flush_period = stdout_flush_period;
  }
};

}  // namespace lyra::runtime
