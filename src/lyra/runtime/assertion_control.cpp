#include "lyra/runtime/assertion_control.hpp"

#include <cstddef>
#include <cstdint>
#include <new>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/deferred_effects.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/hierarchy_levels.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/support/assertion_control.hpp"

namespace lyra::runtime {

namespace {

// What a statement of `type` gives the tasks to act on (LRM 20.11): an
// assertion states a directive and has both checking and actions, an expect
// statement has actions and no checking a task turns off, and a violation
// report has checking and no actions.
struct Controllable {
  bool states_directive;
  bool checks;
  bool acts;
};

auto ControllableOf(support::AssertionType type) -> Controllable {
  switch (type) {
    case support::AssertionType::kConcurrent:
    case support::AssertionType::kSimpleImmediate:
    case support::AssertionType::kObservedDeferredImmediate:
    case support::AssertionType::kFinalDeferredImmediate:
      return {.states_directive = true, .checks = true, .acts = true};
    case support::AssertionType::kExpect:
      return {.states_directive = false, .checks = false, .acts = true};
    case support::AssertionType::kUnique:
    case support::AssertionType::kUnique0:
    case support::AssertionType::kPriority:
      return {.states_directive = false, .checks = true, .acts = false};
  }
  throw InternalError("AssertionControl: unknown assertion type");
}

// Whether `control` reaches a statement of `type` at all (LRM 20.11): On, Off
// and Kill reach what checks, and the action controls what acts.
auto Reaches(support::AssertionControlType control, support::AssertionType type)
    -> bool {
  const Controllable controllable = ControllableOf(type);
  switch (control) {
    case support::AssertionControlType::kLock:
    case support::AssertionControlType::kUnlock:
      return true;
    case support::AssertionControlType::kOn:
    case support::AssertionControlType::kOff:
    case support::AssertionControlType::kKill:
      return controllable.checks;
    case support::AssertionControlType::kPassOn:
    case support::AssertionControlType::kPassOff:
    case support::AssertionControlType::kFailOn:
    case support::AssertionControlType::kFailOff:
    case support::AssertionControlType::kNonvacuousOn:
    case support::AssertionControlType::kVacuousOff:
      return controllable.acts;
  }
  throw InternalError("AssertionControl: unknown control type");
}

}  // namespace

AssertionControl::AssertionControl() = default;
AssertionControl::~AssertionControl() = default;

void AssertionControl::RegisterIn(
    RuntimeEffects& effects, Scope* in, std::uint64_t type,
    std::uint64_t directive) {
  Describe(effects, type, directive);
  effects.AssertionControlsOfTheRun().AddIn(*this, *in);
}

void AssertionControl::Register(
    RuntimeEffects& effects, std::uint64_t type, std::uint64_t directive) {
  Describe(effects, type, directive);
  effects.AssertionControlsOfTheRun().Add(*this);
}

void AssertionControl::Describe(
    RuntimeEffects& effects, std::uint64_t type, std::uint64_t directive) {
  effects_ = &effects;
  type_ = static_cast<support::AssertionType>(type);
  directive_ = directive;
}

void AssertionControl::RegisterWithin(const CancellationTarget* block) {
  Effects().AssertionControlsOfTheRun().AddWithin(*this, *block);
}

void AssertionControl::Watch(EvaluationAttempts& attempts) {
  attempts_ = &attempts;
}

auto AssertionControl::SelectedBy(
    std::uint64_t types, std::uint64_t directives) const -> bool {
  if ((types & static_cast<std::uint64_t>(type_)) == 0) {
    return false;
  }
  return !ControllableOf(type_).states_directive ||
         (directives & directive_) != 0;
}

void AssertionControl::Apply(support::AssertionControlType control) {
  if (!Reaches(control, type_)) {
    return;
  }
  if (control == support::AssertionControlType::kUnlock) {
    locked_ = false;
    return;
  }
  if (locked_) {
    return;
  }
  const auto set = [&](std::uint64_t bits, bool on) {
    allowed_ = on ? (allowed_ | bits) : (allowed_ & ~bits);
  };
  const std::uint64_t passes = support::kAssertionPassesVacuously |
                               support::kAssertionPassesNonvacuously;
  switch (control) {
    case support::AssertionControlType::kLock:
      locked_ = true;
      return;
    case support::AssertionControlType::kUnlock:
      return;
    case support::AssertionControlType::kOn:
      set(support::kAssertionChecks, true);
      return;
    case support::AssertionControlType::kOff:
      set(support::kAssertionChecks, false);
      return;
    case support::AssertionControlType::kKill:
      set(support::kAssertionChecks, false);
      ++generation_;
      if (attempts_ != nullptr) {
        attempts_->Discard();
      }
      return;
    case support::AssertionControlType::kPassOn:
      set(passes, true);
      return;
    case support::AssertionControlType::kPassOff:
      set(passes, false);
      return;
    case support::AssertionControlType::kFailOn:
      set(support::kAssertionFails, true);
      return;
    case support::AssertionControlType::kFailOff:
      set(support::kAssertionFails, false);
      return;
    case support::AssertionControlType::kNonvacuousOn:
      set(support::kAssertionPassesNonvacuously, true);
      return;
    case support::AssertionControlType::kVacuousOff:
      set(support::kAssertionPassesVacuously, false);
      return;
  }
  throw InternalError("AssertionControl::Apply: unknown control type");
}

void AssertionControl::SubmitObserved(Coroutine<void> body) {
  Effects().SubmitDeferredObserved(std::move(body), *this);
}

void AssertionControl::SubmitFinal(Coroutine<void> body) {
  Effects().SubmitDeferredFinal(std::move(body), *this);
}

auto AssertionControl::Allocate(std::size_t size, std::size_t alignment)
    -> void* {
  return Effects().ObservedEffects()->Allocate(size, alignment);
}

void AssertionControl::Commit(void* made) {
  // LRM 20.11: with checking off no new violation report is queued, so the one
  // just made is ended unqueued.
  if ((allowed_ & support::kAssertionChecks) == 0) {
    const OwnedEffect dropped = Effects().ObservedEffects()->Take(made);
    return;
  }
  Effects().KeepViolationReport(made, *this);
}

auto AssertionControl::Effects() const -> RuntimeEffects& {
  if (effects_ == nullptr) {
    throw InternalError(
        "AssertionControl: a statement was reached before the design "
        "registered its control");
  }
  return *effects_;
}

void AssertionControls::AddIn(AssertionControl& control, const Scope& in) {
  by_scope_[&in].push_back(&control);
}

void AssertionControls::Add(AssertionControl& control) {
  unscoped_.push_back(&control);
}

void AssertionControls::AddWithin(
    AssertionControl& control, const CancellationTarget& block) {
  by_block_[&block].push_back(&control);
}

void AssertionControls::ApplyToDesign(
    Scope& root, support::AssertionControlType control, std::uint64_t types,
    std::uint64_t directives, std::int64_t levels) {
  ApplyEach(unscoped_, control, types, directives);
  // A top's own scope is the outermost the design declares: the root above the
  // tops is no instance of it (LRM 23.3.1).
  root.ForEachChild([&](Scope& top) {
    Walk(top, LevelsNamed(levels), control, types, directives);
  });
}

void AssertionControls::ApplyToScope(
    Scope& scope, support::AssertionControlType control, std::uint64_t types,
    std::uint64_t directives, std::int64_t levels) {
  Walk(scope, LevelsNamed(levels), control, types, directives);
}

void AssertionControls::ApplyToBlock(
    const CancellationTarget& block, support::AssertionControlType control,
    std::uint64_t types, std::uint64_t directives) {
  if (const auto found = by_block_.find(&block); found != by_block_.end()) {
    ApplyEach(found->second, control, types, directives);
  }
}

void AssertionControls::Walk(
    Scope& scope, std::int64_t remaining, support::AssertionControlType control,
    std::uint64_t types, std::uint64_t directives) {
  if (remaining == 0) {
    return;
  }
  if (const auto found = by_scope_.find(&scope); found != by_scope_.end()) {
    ApplyEach(found->second, control, types, directives);
  }
  scope.ForEachChild([&](Scope& child) {
    Walk(child, LevelsBelow(child, remaining), control, types, directives);
  });
}

void AssertionControls::ApplyEach(
    const std::vector<AssertionControl*>& controls,
    support::AssertionControlType control, std::uint64_t types,
    std::uint64_t directives) {
  for (AssertionControl* each : controls) {
    if (each->SelectedBy(types, directives)) {
      each->Apply(control);
    }
  }
}

}  // namespace lyra::runtime

auto operator new(std::size_t size, lyra::runtime::AssertionControl* control)
    -> void* {
  return control->Allocate(size, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}

auto operator new(
    std::size_t size, std::align_val_t alignment,
    lyra::runtime::AssertionControl* control) -> void* {
  return control->Allocate(size, static_cast<std::size_t>(alignment));
}
