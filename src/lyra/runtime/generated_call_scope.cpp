#include "lyra/runtime/generated_call_scope.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

namespace {

auto CurrentScopeSlot() -> GeneratedCallScope*& {
  static thread_local GeneratedCallScope* scope = nullptr;
  return scope;
}

}  // namespace

GeneratedCallScope::GeneratedCallScope()
    : previous_(CurrentScopeSlot()),
      activation_frame_(
          previous_ != nullptr ? previous_->activation_frame_ : nullptr) {
  CurrentScopeSlot() = this;
}

GeneratedCallScope::GeneratedCallScope(ActivationFrameStorage* activation_frame)
    : previous_(CurrentScopeSlot()), activation_frame_(activation_frame) {
  CurrentScopeSlot() = this;
}

GeneratedCallScope::~GeneratedCallScope() {
  CurrentScopeSlot() = previous_;
}

auto GeneratedCallScope::ActivationFrame() -> ActivationFrameStorage& {
  if (activation_frame_ == nullptr) {
    throw InternalError(
        "generated call: no activation frame; a cross-suspension value was "
        "requested outside a suspending body");
  }
  return *activation_frame_;
}

auto GeneratedCallScope::Current() -> GeneratedCallScope& {
  if (CurrentScopeSlot() == nullptr) {
    throw InternalError("generated call: no active call scope");
  }
  return *CurrentScopeSlot();
}

}  // namespace lyra::runtime
