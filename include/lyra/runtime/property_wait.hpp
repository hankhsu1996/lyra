#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// The one evaluation an expect statement starts (LRM 16.17), told of what it
// advances on while its process waits: each occurrence of the clock is one
// more tick for the Observed region of the time step it happened in to
// evaluate (LRM 16.5), and each change to what the disable condition reads is
// one more moment that condition is tested at, in the Active region after it
// (LRM 16.12). Neither wakes anyone; the process is woken once, by the answer,
// which lands it after the Observed region that settled it. What a tick and a
// test run is code of the waiting body, bound to its frame, which the frame
// outlives because this is held by a wait in it -- ending the wait takes
// whatever is still queued off its queue.
class PropertyEvaluation {
 public:
  PropertyEvaluation(
      RuntimeEffects& effects, std::span<const Trigger* const> clock,
      std::span<const Trigger* const> disable_reads, std::uint64_t words);

  PropertyEvaluation(const PropertyEvaluation&) = delete;
  auto operator=(const PropertyEvaluation&) -> PropertyEvaluation& = delete;
  // Non-movable: the places it watches point at its memberships.
  PropertyEvaluation(PropertyEvaluation&&) = delete;
  auto operator=(PropertyEvaluation&&) -> PropertyEvaluation& = delete;
  ~PropertyEvaluation() = default;

  // What a tick and a test of the disable condition run, given once the wait
  // holding this exists, since each is code of the waiting body that reaches
  // back into it.
  void Bind(OwnedCall step, OwnedCall test);

  // The disable condition held: the evaluation answers nothing, so nothing is
  // evaluated again and the process is never woken (LRM 16.12, 16.17).
  void Disable();

  // An occurrence reached one of the places this watches.
  void Reached(const WaitMembership& membership);

  [[nodiscard]] auto Attempts() -> EvaluationAttempts& {
    return attempts_;
  }

  // Whether the attempt has succeeded or failed.
  [[nodiscard]] auto Answered() const -> bool;

  // Where the answer is reported, which the wait enrols on.
  [[nodiscard]] auto Answers() -> IntrusiveList<WaitMembership>& {
    return answers_;
  }

 private:
  void Watch(const Trigger& trigger);
  void Step();

  RuntimeEffects* effects_;
  EvaluationAttempts attempts_;
  // One block, never resized, because the places watched point at these.
  std::vector<WaitMembership> memberships_;
  std::size_t watched_ = 0;
  // How many of the memberships, from the first, are the clock's, which is
  // what tells a tick from a change the disable condition reads.
  std::size_t clock_watched_ = 0;
  IntrusiveList<WaitMembership> answers_;
  OwnedCall step_;
  Timer stepping_;
  Timer testing_;
  // The ticks the clock reached since the Observed region last stepped.
  std::uint64_t pending_ticks_ = 0;
  bool disabled_ = false;
};

// The wait an expect statement stops at, holding its evaluation over position
// sets `words` wide and woken by its answer alone. The leaves are read here
// and not kept, laid out in one array or each where its builder left it.
auto WaitOnPropertyEvaluation(
    RuntimeEffects& effects, std::span<const Trigger> clock,
    std::span<const Trigger> disable_reads, std::uint64_t words) -> Wait;
auto WaitOnPropertyEvaluation(
    RuntimeEffects& effects, std::span<const Trigger* const> clock,
    std::span<const Trigger* const> disable_reads, std::uint64_t words) -> Wait;

// The attempt the wait an expect statement built holds.
auto AttemptsOf(Wait* wait) -> EvaluationAttempts*;

// Hands the wait the tick and the disable test its evaluation runs.
void BindPropertyEvaluation(Wait* wait, OwnedCall step, OwnedCall test);

// The disable condition held, for the wait's evaluation.
void DisablePropertyEvaluation(Wait* wait);

}  // namespace lyra::runtime
