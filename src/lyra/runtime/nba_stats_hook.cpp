#include "lyra/runtime/nba_stats_hook.hpp"

namespace lyra::runtime {

namespace {

auto CallbackStorage() -> NbaStatsCallback& {
  thread_local NbaStatsCallback instance;
  return instance;
}

}  // namespace

void SetNbaStatsCallback(NbaStatsCallback callback) {
  CallbackStorage() = std::move(callback);
}

auto GetNbaStatsCallback() -> NbaStatsCallback {
  return CallbackStorage();
}

}  // namespace lyra::runtime
