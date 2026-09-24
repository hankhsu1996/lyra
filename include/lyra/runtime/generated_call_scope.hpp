#pragma once

#include <exception>
#include <memory>
#include <utility>
#include <vector>

namespace lyra::runtime {

// One execution's value storage: the cells a body reaches across suspensions,
// and it is only the values. The frame -- locals, resume state -- belongs to
// the generated body and this is not it, which is why it is not named one.
//
// Its life is one execution's, so a value here outlives every stretch of the
// body that reads it and is released when that execution ends.
class ActivationValueStore {
 public:
  template <typename T, typename... Args>
  auto New(Args&&... args) -> T* {
    auto owner = std::make_shared<T>(std::forward<Args>(args)...);
    T* value = owner.get();
    objects_.push_back(std::move(owner));
    return value;
  }

 private:
  std::vector<std::shared_ptr<void>> objects_;
};

// The running execution, as the generated code of one stretch of it reaches
// it: the stretch allocates a value whose lifetime crosses a suspension in the
// execution's store, and settles a departure that leaves the body where the
// execution carries it on once the body has completed, without being handed
// either -- so the runtime names both around each stretch it runs. The
// generated IR never names the scope. Both are borrowed from whatever drives
// the execution, and a scope is open for one stretch only, since one left open
// across a park would still be the innermost while some other execution ran.
// A stretch no generated body completes into -- a foreign call's -- names no
// place to settle a departure.
class GeneratedCallScope {
 public:
  GeneratedCallScope(
      ActivationValueStore& values, std::exception_ptr* departure);
  ~GeneratedCallScope();
  GeneratedCallScope(const GeneratedCallScope&) = delete;
  auto operator=(const GeneratedCallScope&) -> GeneratedCallScope& = delete;
  GeneratedCallScope(GeneratedCallScope&&) = delete;
  auto operator=(GeneratedCallScope&&) -> GeneratedCallScope& = delete;

  auto ActivationValues() -> ActivationValueStore&;

  // Settling where no body completes into the execution is a lowering defect
  // -- only a suspending body does -- so it throws.
  void SettleDeparture(std::exception_ptr departure);

  // The innermost open scope. There is none outside a stretch of a suspending
  // body, and only such a body has cross-suspension values or settles a
  // departure, so reaching for one there is a lowering defect and throws.
  static auto Current() -> GeneratedCallScope&;

 private:
  GeneratedCallScope* previous_;
  ActivationValueStore* values_;
  std::exception_ptr* departure_;
};

}  // namespace lyra::runtime
