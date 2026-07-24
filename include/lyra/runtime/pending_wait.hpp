#pragma once

#include <cstdint>

#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

class RuntimeEffects;

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
};

}  // namespace lyra::runtime
