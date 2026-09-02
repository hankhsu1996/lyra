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
      values_(previous_ != nullptr ? previous_->values_ : nullptr) {
  CurrentScopeSlot() = this;
}

GeneratedCallScope::GeneratedCallScope(ActivationValueStore* values)
    : previous_(CurrentScopeSlot()), values_(values) {
  CurrentScopeSlot() = this;
}

GeneratedCallScope::~GeneratedCallScope() {
  CurrentScopeSlot() = previous_;
}

auto GeneratedCallScope::ActivationValues() -> ActivationValueStore& {
  if (values_ == nullptr) {
    throw InternalError(
        "generated call: no value store; a cross-suspension value was "
        "requested outside a suspending body");
  }
  return *values_;
}

auto GeneratedCallScope::Current() -> GeneratedCallScope& {
  if (CurrentScopeSlot() == nullptr) {
    throw InternalError("generated call: no active call scope");
  }
  return *CurrentScopeSlot();
}

}  // namespace lyra::runtime
