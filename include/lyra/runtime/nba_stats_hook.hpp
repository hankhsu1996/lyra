#pragma once

#include <cstdint>
#include <functional>

namespace lyra::runtime {

// NBA routing stats captured after simulation for test assertions.
struct NbaRoutingStats {
  uint64_t generic_queue = 0;
  uint64_t deferred_local = 0;
};

// Callback for capturing NBA routing stats after simulation.
// Called by LyraRunSimulation before engine destruction.
using NbaStatsCallback = std::function<void(NbaRoutingStats)>;

void SetNbaStatsCallback(NbaStatsCallback callback);
auto GetNbaStatsCallback() -> NbaStatsCallback;

// RAII guard for the NBA stats callback.
class NbaStatsCallbackScope {
 public:
  explicit NbaStatsCallbackScope(NbaStatsCallback callback)
      : prev_(GetNbaStatsCallback()) {
    SetNbaStatsCallback(std::move(callback));
  }
  ~NbaStatsCallbackScope() {
    SetNbaStatsCallback(std::move(prev_));
  }

  NbaStatsCallbackScope(const NbaStatsCallbackScope&) = delete;
  NbaStatsCallbackScope(NbaStatsCallbackScope&&) = delete;
  auto operator=(const NbaStatsCallbackScope&)
      -> NbaStatsCallbackScope& = delete;
  auto operator=(NbaStatsCallbackScope&&) -> NbaStatsCallbackScope& = delete;

 private:
  NbaStatsCallback prev_;
};

}  // namespace lyra::runtime
