#include "lyra/runtime/generated_call_scope.hpp"

#include <exception>
#include <utility>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

namespace {

auto CurrentScopeSlot() -> GeneratedCallScope*& {
  static thread_local GeneratedCallScope* scope = nullptr;
  return scope;
}

}  // namespace

GeneratedCallScope::GeneratedCallScope(
    ActivationValueStore& values, std::exception_ptr* departure)
    : previous_(CurrentScopeSlot()), values_(&values), departure_(departure) {
  CurrentScopeSlot() = this;
}

GeneratedCallScope::~GeneratedCallScope() {
  CurrentScopeSlot() = previous_;
}

auto GeneratedCallScope::ActivationValues() -> ActivationValueStore& {
  return *values_;
}

void GeneratedCallScope::SettleDeparture(std::exception_ptr departure) {
  if (departure_ == nullptr) {
    throw InternalError(
        "generated call: a departure was settled outside a suspending body");
  }
  *departure_ = std::move(departure);
}

auto GeneratedCallScope::Current() -> GeneratedCallScope& {
  if (CurrentScopeSlot() == nullptr) {
    throw InternalError(
        "generated call: no execution is open; a cross-suspension value or a "
        "departure was reached for outside a suspending body");
  }
  return *CurrentScopeSlot();
}

}  // namespace lyra::runtime
