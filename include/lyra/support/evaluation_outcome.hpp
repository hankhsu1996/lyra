#pragma once

#include <cstdint>

namespace lyra::support {

// What one tick left one evaluation of a property in (Annex F.5.3.2's four
// answers, read as a schedule rather than as a verdict).
//
// The generated transition decides it rather than the runtime, because which
// positions carry something the attempt owes and which carry the antecedent of
// an implication is settled where the automaton is built. What holds the
// evaluations only ever sees this answer.
enum class EvaluationOutcome : std::uint8_t {
  // Some path survives the tick, so the trace has not settled this evaluation.
  kLive,
  // A path reached the end of what was owed here. A sequence's matching is
  // existential, so one match is the whole question and nothing further is
  // asked of this evaluation.
  kSatisfied,
  // No path survives and a match was owed, which makes the whole attempt false
  // -- one failure settles a conjunction.
  kFailed,
  // No path survives and none was owed: an implication's antecedent never
  // matched, so the implication is true without its consequent being read at
  // all (LRM 16.12.7).
  kVacuous,
};

// What one attempt as a whole has come to so far, from the outcomes of the
// evaluations it holds: pending until one of them fails or every one of them
// is satisfied, then failing or holding. An expect statement's process reads
// it to learn whether to go on and which arm to run (LRM 16.17).
enum class AttemptAnswer : std::uint8_t {
  kPending,
  kHolds,
  kFails,
};

}  // namespace lyra::support
