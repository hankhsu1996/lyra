#include "lyra/runtime/coroutine.hpp"

#include <coroutine>
#include <exception>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

PromiseBase::~PromiseBase() = default;

auto PromiseBase::Park(RegistrationList& target) -> Registration& {
  Registration& reg = registrations.emplace_back();
  reg.activation = this;
  target.PushBack(reg);
  return reg;
}

void PromiseBase::RevokeRegistrations() noexcept {
  registrations.clear();
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
auto PromiseBase::initial_suspend() noexcept -> std::suspend_always {
  return {};
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
auto PromiseBase::FinalAwaiter::await_ready() const noexcept -> bool {
  return false;
}

auto PromiseBase::FinalAwaiter::await_suspend(
    // NOLINTNEXTLINE(readability-named-parameter)
    std::coroutine_handle<>) const noexcept -> std::coroutine_handle<> {
  if (promise->on_complete) {
    promise->on_complete();
  }
  if (promise->continuation) {
    return promise->continuation;
  }
  return std::noop_coroutine();
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
void PromiseBase::FinalAwaiter::await_resume() const noexcept {
}

auto PromiseBase::final_suspend() noexcept -> FinalAwaiter {
  return FinalAwaiter{.promise = this};
}

auto PromiseBase::Process() const -> RuntimeProcess& {
  if (process == nullptr) {
    throw InternalError(
        "PromiseBase::Process: no RuntimeProcess back-pointer set");
  }
  return *process;
}

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
