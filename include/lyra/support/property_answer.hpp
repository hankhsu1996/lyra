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
// reaches either end of the order. An expect statement's process reads it to
// learn whether to go on and which arm to run (LRM 16.17).
enum class AttemptAnswer : std::uint8_t {
  kPending,
  kHolds,
  kFails,
};

}  // namespace lyra::support
