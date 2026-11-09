#pragma once

#include <cstdint>

namespace lyra::support {

// What a property has come to on the trace so far, under the names Annex
// F.5.3.2 gives the four answers a finished trace admits, in order. The two
// ends are settled: no continuation of the trace can change a property that
// fails or holds strongly, so an attempt reports there. The two between are
// what the trace as it stands says, read as F.5.3.1 reads a finished trace,
// which is what an attempt the run ends in the middle of comes to. Negation
// reverses the order; a conjunction is the least of its operands' answers and a
// disjunction the greatest.
//
// The generated code computing a property's answer decides it, because which
// positions belong to which part of the property is settled where the automaton
// is built. What holds the evaluations only orders answers and reads their two
// ends.
enum class PropertyAnswer : std::uint8_t {
  // No continuation of the trace can satisfy it.
  kFails,
  // The trace as it stands does not satisfy it, and a continuation may.
  kPending,
  // The trace as it stands satisfies it, and a continuation may not.
  kHolds,
  // No continuation of the trace can fail it.
  kHoldsStrongly,
};

// What one attempt as a whole has come to so far: pending until its answer
// reaches either end of the order, or disabled, which is neither a success nor
// a failure (LRM 16.12). An expect statement's process reads it to learn
// whether to go on and which arm to run, and is never woken by a disabled one
// (LRM 16.17).
enum class AttemptAnswer : std::uint8_t {
  kPending,
  kHolds,
  kFails,
  kDisabled,
};

// What the closure of one attempt of a concurrent assertion is called for
// (LRM 16.14.6). The closure is the code of the attempt bound to the values it
// was made with -- those a procedural assertion saved where it was queued (LRM
// 16.14.6.1) -- so whatever reads those values is asked of it: a tick of its
// clock, a test of its disable condition between ticks (LRM 16.12), and the
// statement its answer selects (LRM 16.14.1).
enum class AttemptEvent : std::uint8_t {
  // The clock ticked: step the attempt's evaluations, unless the disable
  // condition holds, which the call answers with 1.
  kTick,
  // What the disable condition reads changed: answers 1 where it holds.
  kDisableTest,
  // The attempt succeeded, and its pass statement may run: start it.
  kSucceeded,
  // The attempt failed, and its fail statement may run: start it.
  kFailed,
};

}  // namespace lyra::support
