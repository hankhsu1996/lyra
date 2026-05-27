#include "lyra/runtime/runtime_services.hpp"

#include <functional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

void RuntimeServices::SubmitNba(std::function<void()> closure) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::SubmitNba: no Engine bound");
  }
  engine_->SubmitNba(std::move(closure));
}

}  // namespace lyra::runtime
