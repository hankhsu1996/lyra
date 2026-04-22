#pragma once

#include <cstdint>
#include <functional>
#include <vector>

namespace lyra::runtime {

// Callback type for capturing cover hit counts after simulation.
// Called with the per-site hit count array (indexed by CoverSiteId).
using CoverHitCallback = std::function<void(std::vector<uint64_t>)>;

// Set/get the thread-local cover hit callback. When set, LyraRunSimulation
// invokes it with the engine's cover hit counts before the engine is
// destroyed. Used by the test framework to verify per-site hit recording.
void SetCoverHitCallback(CoverHitCallback callback);
auto GetCoverHitCallback() -> CoverHitCallback;

// RAII guard for the cover hit callback.
class CoverHitCallbackScope {
 public:
  explicit CoverHitCallbackScope(CoverHitCallback callback)
      : prev_(GetCoverHitCallback()) {
    SetCoverHitCallback(std::move(callback));
  }
  ~CoverHitCallbackScope() {
    SetCoverHitCallback(std::move(prev_));
  }

  CoverHitCallbackScope(const CoverHitCallbackScope&) = delete;
  CoverHitCallbackScope(CoverHitCallbackScope&&) = delete;
  auto operator=(const CoverHitCallbackScope&)
      -> CoverHitCallbackScope& = delete;
  auto operator=(CoverHitCallbackScope&&) -> CoverHitCallbackScope& = delete;

 private:
  CoverHitCallback prev_;
};

}  // namespace lyra::runtime
