#pragma once

#include <cstdint>

#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class RuntimeProcess;

// Whether arranging a wait found what it waits for already done.
enum class WaitOutcome : std::uint8_t {
  // Arranged: the execution stays parked until it happens.
  kBlocked,
  // Already done: nothing was arranged and the execution carries on.
  kSatisfied,
};

// What an execution is waiting for, held by the execution rather than by the
// statement that asked for it. That ownership is what LRM 9.7 requires: a
// process stopped from outside is desensitized to what it was blocked on, and
// starting it again waits for the same thing afresh -- without running a
// statement of the body, which is the only other thing that knows what that
// was. So the moment the body parks is the last moment anything can record it.
//
// Every suspending construct has one, and the scheduler reaches all of them
// through this surface without asking which construct a wait came from.
class Wait {
 public:
  Wait() = default;
  Wait(const Wait&) = delete;
  auto operator=(const Wait&) -> Wait& = delete;
  Wait(Wait&&) = delete;
  auto operator=(Wait&&) -> Wait& = delete;
  virtual ~Wait() = default;

  // Arrange for `leaf` to continue when what this waits for happens, answering
  // whether it had already happened -- in which case nothing was arranged.
  virtual auto Begin(RuntimeEffects& services, CoroutineHandle leaf)
      -> WaitOutcome = 0;

  // The same, after the execution was stopped from outside and started again
  // (LRM 9.7). The standard gives each construct its own rule there, and for
  // most of them it is this same arrangement made afresh: an event control
  // subscribes again, so an occurrence during the stop is missed, and a delay
  // compares the deadline it is already holding.
  virtual auto Again(RuntimeEffects& services, CoroutineHandle leaf)
      -> WaitOutcome {
    return Begin(services, leaf);
  }

  // Whether a process resuming from this wait reaches a violation report flush
  // point (LRM 12.4.2.1), discarding the reports it still has pending. The LRM
  // names two: resuming from an event control or a wait statement, and an
  // always_comb / always_latch resumed by a transition on what it reads. Time
  // passing is not one of them, so each construct answers for its own
  // suspension rather than the resume path guessing from the queue it came off.
  [[nodiscard]] virtual auto IsReportFlushPoint() const -> bool = 0;
};

// The dual of parking on a wait: `activation` is runnable now, so it holds no
// membership and no wait until its body parks again, and nothing it was
// enrolled on -- the sibling observables of an `@(a or b)`, the event it waited
// for -- can fire it a second time. A wait the LRM counts as a violation report
// flush point clears its process's report queue on the way out (LRM 12.4.2.1);
// the activation cannot run between here and its resume, so discarding at
// either point is the same discard.
void ConsumeWait(CoroutineHandle activation);

}  // namespace lyra::runtime
