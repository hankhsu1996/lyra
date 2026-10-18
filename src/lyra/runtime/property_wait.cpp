#include "lyra/runtime/property_wait.hpp"

#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/support/evaluation_outcome.hpp"

namespace lyra::runtime {

PropertyEvaluation::PropertyEvaluation(
    RuntimeEffects& effects, std::span<const Trigger* const> clock,
    std::span<const Trigger* const> disable_reads, std::uint64_t words)
    : effects_(&effects), memberships_(clock.size() + disable_reads.size()) {
  attempts_.Hold(words);
  for (const Trigger* trigger : clock) {
    Watch(*trigger);
  }
  clock_watched_ = watched_;
  for (const Trigger* trigger : disable_reads) {
    Watch(*trigger);
  }
  // What a tick is measured from is what each clock expression is worth where
  // the evaluation begins (LRM 9.4.2).
  for (std::size_t watched = 0; watched < clock_watched_; ++watched) {
    memberships_[watched].observation.Arm();
  }
  stepping_.Bind([this] { Step(); });
}

void PropertyEvaluation::Watch(const Trigger& trigger) {
  // Storage that belongs to nothing is never told of a write, so there is
  // nothing to enrol on.
  if (trigger.observable == nullptr) {
    return;
  }
  WaitMembership& membership = memberships_[watched_++];
  membership.observer = this;
  membership.through = trigger.through;
  membership.reads = trigger.reads;
  membership.observation = trigger.observation;
  trigger.observable->MembersAt(trigger.part).PushBack(membership);
}

void PropertyEvaluation::Bind(OwnedCall step, OwnedCall test) {
  step_ = std::move(step);
  testing_.Bind(std::move(test));
}

void PropertyEvaluation::Disable() {
  disabled_ = true;
  attempts_.DisableTick();
  pending_ticks_ = 0;
  stepping_.Place().Unlink();
  testing_.Place().Unlink();
}

auto PropertyEvaluation::Answered() const -> bool {
  switch (static_cast<support::AttemptAnswer>(attempts_.Answer())) {
    case support::AttemptAnswer::kPending:
      return false;
    case support::AttemptAnswer::kHolds:
    case support::AttemptAnswer::kFails:
      return true;
  }
  throw InternalError("PropertyEvaluation: unknown attempt answer");
}

void PropertyEvaluation::Reached(const WaitMembership& membership) {
  if (disabled_ || Answered() || !membership.observation.Fires()) {
    return;
  }
  const auto position =
      static_cast<std::size_t>(&membership - memberships_.data());
  if (position < clock_watched_) {
    ++pending_ticks_;
    stepping_.ArmNow(*effects_, Region::kObserved);
    return;
  }
  testing_.ArmNow(*effects_, Region::kActive);
}

void PropertyEvaluation::Step() {
  // Every tick the clock reached since the last of these is a step of its
  // own, all over this time step's sampled values.
  while (pending_ticks_ != 0 && !disabled_ && !Answered()) {
    --pending_ticks_;
    step_();
  }
  pending_ticks_ = 0;
  if (Answered()) {
    effects_->WakeParkedOn(answers_, Change::Whole());
  }
}

namespace {

// Waiting for the answer of the evaluation it holds.
class PropertyEvaluationAwaiter final : public StateAwaiter {
 public:
  PropertyEvaluationAwaiter(
      RuntimeEffects& effects, std::span<const Trigger* const> clock,
      std::span<const Trigger* const> disable_reads, std::uint64_t words)
      : StateAwaiter(1), evaluation_(effects, clock, disable_reads, words) {
    EnrolOn(evaluation_.Answers());
  }

  // LRM 12.4.2.1 names an event control and a wait statement as the points a
  // process flushes its violation reports at, and an expect is neither.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

  [[nodiscard]] auto Evaluation() -> PropertyEvaluation& {
    return evaluation_;
  }

 private:
  [[nodiscard]] auto Holds() const -> bool override {
    return evaluation_.Answered();
  }

  PropertyEvaluation evaluation_;
};

auto EvaluationOf(Wait* wait) -> PropertyEvaluation& {
  auto* awaiter = dynamic_cast<PropertyEvaluationAwaiter*>(&wait->Awaited());
  if (awaiter == nullptr) {
    throw InternalError("the wait was not built by an expect statement");
  }
  return awaiter->Evaluation();
}

}  // namespace

auto WaitOnPropertyEvaluation(
    RuntimeEffects& effects, std::span<const Trigger> clock,
    std::span<const Trigger> disable_reads, std::uint64_t words) -> Wait {
  std::vector<const Trigger*> clock_leaves;
  for (const Trigger& trigger : clock) {
    clock_leaves.push_back(&trigger);
  }
  std::vector<const Trigger*> disable_leaves;
  for (const Trigger& trigger : disable_reads) {
    disable_leaves.push_back(&trigger);
  }
  return WaitOnPropertyEvaluation(effects, clock_leaves, disable_leaves, words);
}

auto WaitOnPropertyEvaluation(
    RuntimeEffects& effects, std::span<const Trigger* const> clock,
    std::span<const Trigger* const> disable_reads, std::uint64_t words)
    -> Wait {
  return MakeWait<PropertyEvaluationAwaiter>(
      effects, clock, disable_reads, words);
}

auto AttemptsOf(Wait* wait) -> EvaluationAttempts* {
  return &EvaluationOf(wait).Attempts();
}

void BindPropertyEvaluation(Wait* wait, OwnedCall step, OwnedCall test) {
  EvaluationOf(wait).Bind(std::move(step), std::move(test));
}

void DisablePropertyEvaluation(Wait* wait) {
  EvaluationOf(wait).Disable();
}

}  // namespace lyra::runtime
