#pragma once

#include <cstdint>

#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class RuntimeProcess;

// Whether re-establishing a wait found its condition already satisfied.
enum class PendingWaitOutcome : std::uint8_t {
  // Re-enrolled; the activation stays parked until the condition fires.
  kReblocked,
  // Already satisfied; the caller schedules the activation to run.
  kRunnable,
};

// The retainable, uniform capability a blocked activation holds to re-establish
// its wait. It is distinct from the `Registration`, which only records the
// current enrollment: a suspend revokes the registration but keeps the pending
// wait, and a resume re-establishes through it without re-entering the
// suspended body (LRM 9.7 process control).
//
// Each suspending construct's awaiter implements this; the activation core
// holds a pointer to the current one and never branches on which construct it
// came from. The awaiter is the wait's own retained state (deadline,
// observables, target), so this is a capability over that state, not a second
// copy of it.
class PendingWait {
 public:
  PendingWait() = default;
  PendingWait(const PendingWait&) = delete;
  auto operator=(const PendingWait&) -> PendingWait& = delete;
  PendingWait(PendingWait&&) = delete;
  auto operator=(PendingWait&&) -> PendingWait& = delete;
  virtual ~PendingWait() = default;

  // Re-establish this wait for `activation` on resume (LRM 9.7). The body is
  // the construct's own resume rule: an edge or named event re-subscribes (a
  // trigger during suspension is missed), a delay compares its absolute
  // deadline (a delay that transpired resumes runnable), a monotonic condition
  // (join, wait fork, await) re-checks and is runnable if met. Returns
  // kRunnable when the condition already holds -- the caller schedules
  // `activation` -- or kReblocked when it re-enrolled and stays parked.
  virtual auto Reestablish(RuntimeEffects& effects, CoroutineHandle activation)
      -> PendingWaitOutcome = 0;

  // Whether a process resuming from this wait reaches a violation report flush
  // point (LRM 12.4.2.1), discarding the reports it still has pending. The LRM
  // names two: resuming from an event control or a wait statement, and an
  // always_comb / always_latch resumed by a transition on what it reads. Time
  // passing is not one of them, so each construct answers for its own
  // suspension rather than the resume path guessing from the queue it came off.
  [[nodiscard]] virtual auto IsReportFlushPoint() const -> bool = 0;

 protected:
  // Blocks `leaf` on this wait, after the construct has armed its own wakeup.
  // Every suspending construct suspends through here, which is what makes this
  // the one place that knows which execution is waiting -- and therefore the
  // one place able to ask, when the wait resumes, whether that execution was
  // disabled while it waited.
  void BlockOn(CoroutineHandle leaf);

  // Raises the pending control effect, if any, for the execution this wait
  // blocked (LRM 9.6.2). Every construct calls it where its wait resumes, so no
  // execution runs a statement of a target that was disabled while it waited.
  void CheckAbortOnResume() const;

 private:
  RuntimeProcess* waiting_process_ = nullptr;
};

// The dual of blocking on a wait: `activation` is runnable now, so it holds no
// membership and no pending wait until its body parks again, and nothing it was
// enrolled on -- the sibling observables of an `@(a or b)`, the event it waited
// for -- can fire it a second time. A wait the LRM counts as a violation report
// flush point clears its process's report queue on the way out (LRM 12.4.2.1);
// the activation cannot run between here and its resume, so discarding at
// either point is the same discard.
void ConsumeWait(CoroutineHandle activation);

}  // namespace lyra::runtime
