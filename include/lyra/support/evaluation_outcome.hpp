#pragma once

#include <cstdint>

namespace lyra::support {

// What one tick left one evaluation of a concurrent assertion's property in
// (Annex F.5.3.2's four answers, read as a schedule rather than as a verdict).
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

}  // namespace lyra::support
