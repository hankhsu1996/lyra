#include "lyra/runtime/module.hpp"

#include <cstdint>
#include <functional>
#include <utility>

namespace lyra::runtime {

void Module::SubmitObserved(std::uint32_t site_id, std::function<void()> fn) {
  if (site_id >= observed_pending_.size()) {
    observed_pending_.resize(site_id + 1);
  }
  observed_pending_[site_id] = std::move(fn);
}

void Module::DrainObserved() {
  for (auto& fn : observed_pending_) {
    if (fn) {
      fn();
      fn = nullptr;
    }
  }
}

}  // namespace lyra::runtime
