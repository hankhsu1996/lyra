#include "lyra/runtime/cover_hook.hpp"

namespace lyra::runtime {

namespace {

auto CallbackStorage() -> CoverHitCallback& {
  thread_local CoverHitCallback instance;
  return instance;
}

}  // namespace

void SetCoverHitCallback(CoverHitCallback callback) {
  CallbackStorage() = std::move(callback);
}

auto GetCoverHitCallback() -> CoverHitCallback {
  return CallbackStorage();
}

}  // namespace lyra::runtime
