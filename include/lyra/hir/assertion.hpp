#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/timing.hpp"

namespace lyra::hir {

struct SequenceExprId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const SequenceExprId&) const
      -> std::strong_ordering = default;
};

struct PropertyExprId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PropertyExprId&) const
      -> std::strong_ordering = default;
};

// A window of clock ticks, counted from the tick a sequence element is measured
// against (LRM 16.7). Both bounds are numbers: the `$` bound names a window
// with no end, which is what makes an evaluation able to outlive every tick the
// trace has, and a sequence carrying one is refused where it is read rather
// than represented here.
struct TickRange {
  std::uint32_t min = 1;
  std::uint32_t max = 1;
};

// A Boolean expression, which matches over the single tick where it holds (LRM
// 16.7). What it is read against is the sampled value of each variable it names
// (LRM 16.5.1).
struct SequenceBoolean {
  ExprId condition;
};

// `s1 ##[m:n] s2`: `s2` begins somewhere between m and n ticks after `s1` ends,
// where a delay of 0 makes the two share that tick (LRM 16.7). The form written
// with no left operand carries a head that matches at one tick unconditionally,
// which is the reading the standard gives it.
struct SequenceDelay {
  SequenceExprId head;
  SequenceExprId tail;
  TickRange delay;
};

// `s[*m:n]`: the operand matches at successive ticks, end point to start point,
// between m and n times over (LRM 16.9.2).
struct SequenceRepetition {
  SequenceExprId body;
  TickRange count;
};

using SequenceExprData =
    std::variant<SequenceBoolean, SequenceDelay, SequenceRepetition>;

struct SequenceExpr {
  SequenceExprData data;
  diag::SourceSpan span;
};

// Whether a sequence standing as a property needs a match to hold, or only the
// absence of a prefix proving no match can follow (LRM 16.12.2). The source may
// write neither, and then the assertion statement decides: an obligation reads
// its sequence weakly and a coverage goal reads it strongly.
enum class SequenceStrength : std::uint8_t {
  kWeak,
  kStrong,
};

struct PropertySequence {
  SequenceExprId sequence;
  SequenceStrength strength;
};

// Where the consequent's evaluation starts, relative to the end point of the
// antecedent match that triggered it: at that same tick for `|->`, and at the
// next one for `|=>` (LRM 16.12.7).
enum class ImplicationStart : std::uint8_t {
  kSameTick,
  kNextTick,
};

// `s |-> p` and `s |=> p`: the consequent is evaluated once for every match of
// the antecedent, and the implication holds when every one of those evaluations
// does -- including vacuously, when the antecedent never matches at all (LRM
// 16.12.7).
struct PropertyImplication {
  SequenceExprId antecedent;
  PropertyExprId consequent;
  ImplicationStart start;
};

using PropertyExprData = std::variant<PropertySequence, PropertyImplication>;

struct PropertyExpr {
  PropertyExprData data;
  diag::SourceSpan span;
};

// The condition whose truth preempts an evaluation attempt (LRM 16.12), and
// what it is watched through. Two things separate it from everything else an
// assertion reads, and the second is what makes it carry a sensitivity of its
// own: it is read on current values rather than sampled ones, and it is tested
// across the whole interval from the start of an attempt to its end rather than
// at that interval's ticks. Watching for a change is the only way to see a
// condition that rises and falls between two ticks.
struct DisableCondition {
  ExprId condition;
  std::vector<SensitivityEntry> sensitivity;
};

// A property together with what it is evaluated against (LRM 16.12): the
// clocking event whose ticks the evaluation advances over, and the condition
// that preempts an attempt. The clock is always present -- where the source
// writes none it is inferred, and a property for which no rule yields one is
// refused rather than evaluated against a guess (LRM 16.14.6, 14.12).
struct PropertySpec {
  EventControl clock;
  std::optional<DisableCondition> disable;
  PropertyExprId body;
};

}  // namespace lyra::hir
