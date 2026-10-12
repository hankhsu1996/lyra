#include "lyra/runtime/reaction.hpp"

#include <coroutine>
#include <cstdint>

#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

namespace {

// What a setup's answer says, which is all the two entries differ in.
enum class SetupAnswers : std::uint8_t { kWhetherAPassFollows, kWhetherToStop };

// The setup and every pass run in a scope of their own, as each stretch of a
// generated body does, and none of them stops: this is the one place the
// process stops. A departure leaving either leaves this execution as it would
// have left the body.
auto RunReaction(
    void* receiver, ReactionPart setup, ReactionPart pass, SetupAnswers answers)
    -> Coroutine<void> {
  ActivationValueStore values;
  Wait wait;
  const auto in_scope = [&](ReactionPart part) {
    const GeneratedCallScope scope(values, nullptr);
    return part(receiver, &wait);
  };
  const bool answer = in_scope(setup);
  bool must_stop = false;
  switch (answers) {
    case SetupAnswers::kWhetherAPassFollows:
      if (!answer) {
        co_return;
      }
      break;
    case SetupAnswers::kWhetherToStop:
      must_stop = answer;
      break;
  }
  for (;;) {
    if (must_stop) {
      co_await std::suspend_always{};
      // Resuming is a point where this execution regains control, so a target
      // it is inside may have been disabled while it was away.
      TakeDepartureIfDue(current_runtime());
    }
    must_stop = in_scope(pass);
  }
}

}  // namespace

auto EnterReactionRunningFirst(
    void* receiver, ReactionPart setup, ReactionPart pass) -> Coroutine<void> {
  return RunReaction(receiver, setup, pass, SetupAnswers::kWhetherAPassFollows);
}

auto EnterReactionStoppingFirst(
    void* receiver, ReactionPart setup, ReactionPart pass) -> Coroutine<void> {
  return RunReaction(receiver, setup, pass, SetupAnswers::kWhetherToStop);
}

}  // namespace lyra::runtime
