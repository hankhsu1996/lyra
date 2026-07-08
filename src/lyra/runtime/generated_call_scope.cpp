#include "lyra/runtime/generated_call_scope.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

namespace {

auto CurrentScopeSlot() -> GeneratedCallScope*& {
  static thread_local GeneratedCallScope* scope = nullptr;
  return scope;
}

}  // namespace

GeneratedCallScope::GeneratedCallScope() : previous_(CurrentScopeSlot()) {
  CurrentScopeSlot() = this;
}

GeneratedCallScope::~GeneratedCallScope() {
  CurrentScopeSlot() = previous_;
}

auto GeneratedCallScope::Current() -> GeneratedCallScope& {
  if (CurrentScopeSlot() == nullptr) {
    throw InternalError("generated call: no active call scope");
  }
  return *CurrentScopeSlot();
}

}  // namespace lyra::runtime
