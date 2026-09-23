#include "lyra/runtime/suspension.hpp"

#include <coroutine>

#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

Suspension::Suspension(bool parked) : parked_(parked) {
}

auto Suspension::await_ready() const noexcept -> bool {
  return !parked_;
}

// NOLINTNEXTLINE(readability-named-parameter)
void Suspension::await_suspend(std::coroutine_handle<>) noexcept {
}

void Suspension::await_resume() const {
  if (parked_) {
    TakeDepartureIfDue(current_runtime().CurrentProcess());
  }
}

}  // namespace lyra::runtime
