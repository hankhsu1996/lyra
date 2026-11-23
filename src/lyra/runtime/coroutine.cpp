#include "lyra/runtime/coroutine.hpp"

#include <coroutine>
#include <exception>
#include <memory>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/keeps_statistics.hpp"
#include "lyra/runtime/run_work.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

Activation::Activation() {
  queued.activation = this;
}
// An activation made later at the same address is another one, which has not
// run in the region's execution under way.
Activation::~Activation() {
  if constexpr (kKeepsStatistics) {
    TheKeptWork().ran_in_execution.erase(this);
  }
}

FrameActivation::FrameActivation() = default;
FrameActivation::~FrameActivation() = default;

// Nothing of this activation is read once it has been carried on: a frame's
// run that ends its process destroys the frame, and this activation with it.
auto Activation::Run(RuntimeEffects& effects) -> bool {
  RuntimeProcess* const process = ProcessIfMade();
  const std::shared_ptr<RuntimeProcess> held =
      process != nullptr ? process->Shared() : nullptr;
  return CarryOn(effects);
}

void Activation::RunAtTheOccurrence(RuntimeEffects& effects) {
  if (!Run(effects) || effects.TryRunning() == nullptr) {
    return;
  }
  RecordRunningEndedTheRun(effects);
  RaiseUnclaimableEffect();
}

Cancelled::Cancelled(std::exception_ptr effect) : effect(std::move(effect)) {
}
Cancelled::Cancelled(const Cancelled&) = default;
auto Cancelled::operator=(const Cancelled&) -> Cancelled& = default;
Cancelled::Cancelled(Cancelled&&) noexcept = default;
auto Cancelled::operator=(Cancelled&&) noexcept -> Cancelled& = default;
Cancelled::~Cancelled() = default;

Raised::Raised(std::exception_ptr error) : error(std::move(error)) {
}
Raised::Raised(const Raised&) = default;
auto Raised::operator=(const Raised&) -> Raised& = default;
Raised::Raised(Raised&&) noexcept = default;
auto Raised::operator=(Raised&&) noexcept -> Raised& = default;
Raised::~Raised() = default;

void Activation::Queue(IntrusiveList<QueuePlace>& queue) noexcept {
  queued.Unlink();
  queue.PushBack(queued);
}

void Activation::ParkOn(Awaiter& stopped_at) noexcept {
  awaiter = &stopped_at;
  stopped_at.parked_ = this;
}

void Activation::Withdraw() noexcept {
  queued.Unlink();
  if (awaiter != nullptr) {
    awaiter->parked_ = nullptr;
  }
}

auto FrameActivation::ContinuationAfterCompletion() const noexcept
    -> std::coroutine_handle<> {
  if (continuation) {
    return continuation;
  }
  return std::noop_coroutine();
}

auto FrameActivation::Thread() const -> CoroutineThread& {
  if (thread == nullptr) {
    throw InternalError(
        "FrameActivation::Thread: a frame runs in no thread -- please report "
        "this as a bug");
  }
  return *thread;
}

CompletionSlot<void>::CompletionSlot() = default;
CompletionSlot<void>::~CompletionSlot() = default;

void CompletionSlot<void>::return_void() {
  outcome_.emplace<Succeeded>();
}

void CompletionSlot<void>::unhandled_exception() noexcept {
  Unwound unwound = ClassifyUnwind();
  if (unwound.control_effect) {
    outcome_.emplace<Cancelled>(std::move(unwound.raised));
    return;
  }
  outcome_.emplace<Raised>(std::move(unwound.raised));
}

auto CompletionSlot<void>::WasCancelled() const -> bool {
  return std::holds_alternative<Cancelled>(outcome_);
}

auto CompletionSlot<void>::TakeRaisedError() -> std::exception_ptr {
  if (auto* raised = std::get_if<Raised>(&outcome_)) {
    return std::move(raised->error);
  }
  return nullptr;
}

void CompletionSlot<void>::Take() {
  if (const auto* cancelled = std::get_if<Cancelled>(&outcome_)) {
    std::rethrow_exception(cancelled->effect);
  }
  if (auto raised = TakeRaisedError()) {
    std::rethrow_exception(raised);
  }
}

}  // namespace lyra::runtime
