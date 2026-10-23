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
// with no end, and a sequence carrying one is refused where it is read rather
// than represented here.
struct TickRange {
  std::uint32_t min = 1;
  std::uint32_t max = 1;

  auto operator==(const TickRange&) const -> bool = default;
};

// A Boolean expression, which matches over the single tick where it holds (LRM
// 16.7). What it is read against is the sampled value of each variable it names
// (LRM 16.5.1).
struct SequenceBoolean {
  ExprId condition;

  auto operator==(const SequenceBoolean&) const -> bool = default;
};

// `s1 ##[m:n] s2`: `s2` begins somewhere between m and n ticks after `s1` ends,
// where a delay of 0 makes the two share that tick (LRM 16.7). The form written
// with no left operand carries a head that matches at one tick unconditionally,
// which is the reading the standard gives it.
struct SequenceDelay {
  SequenceExprId head;
  SequenceExprId tail;
  TickRange delay;

  auto operator==(const SequenceDelay&) const -> bool = default;
};

// `s[*m:n]`: the operand matches at successive ticks, end point to start point,
// between m and n times over (LRM 16.9.2).
struct SequenceRepetition {
  SequenceExprId body;
  TickRange count;

  auto operator==(const SequenceRepetition&) const -> bool = default;
};

using SequenceExprData =
    std::variant<SequenceBoolean, SequenceDelay, SequenceRepetition>;

struct SequenceExpr {
  SequenceExprData data;
  diag::SourceSpan span;

  auto operator==(const SequenceExpr&) const -> bool = default;
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

  auto operator==(const PropertySequence&) const -> bool = default;
};

// Where the consequent's evaluation starts, relative to the end point of the
// antecedent match that triggered it: at that same tick for `|->` and `#-#`,
// and at the next one for `|=>` and `#=#` (LRM 16.12.7, 16.12.9).
enum class ConsequentStart : std::uint8_t {
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
  ConsequentStart start;

  auto operator==(const PropertyImplication&) const -> bool = default;
};

// `s #-# p` and `s #=# p`: some match of the antecedent is followed by the
// consequent holding from where the match ends, or from the tick after (LRM
// 16.12.9). The dual of an implication, which asks it of every match.
struct PropertyFollowedBy {
  SequenceExprId antecedent;
  PropertyExprId consequent;
  ConsequentStart start;

  auto operator==(const PropertyFollowedBy&) const -> bool = default;
};

// `not p`: true where its operand is false (LRM 16.12.3). Each sequence under
// it keeps the strength written for it, and negation exchanges what that
// strength asks.
struct PropertyNot {
  PropertyExprId operand;

  auto operator==(const PropertyNot&) const -> bool = default;
};

// The connectives combining two properties evaluated from the same start (LRM
// 16.12.4, 16.12.5, 16.12.8).
enum class PropertyConnective : std::uint8_t {
  kAnd,
  kOr,
  kImplies,
  kIff,
};

struct PropertyBinary {
  PropertyConnective connective;
  PropertyExprId lhs;
  PropertyExprId rhs;

  auto operator==(const PropertyBinary&) const -> bool = default;
};

// `if (b) p1 else p2`: the condition is read where the property starts, and the
// property is then the arm it selects. With no else, a false condition makes
// it true (LRM 16.12.6).
struct PropertyConditional {
  ExprId condition;
  PropertyExprId then_property;
  std::optional<PropertyExprId> else_property;

  auto operator==(const PropertyConditional&) const -> bool = default;
};

struct PropertyCaseItem {
  std::vector<ExprId> labels;
  PropertyExprId property;

  auto operator==(const PropertyCaseItem&) const -> bool = default;
};

// `case (e) ... endcase` over properties: the subject is read where the
// property starts and compared with the labels as a case statement compares
// them (LRM 12.5), and the property is the first matching item's, otherwise
// the default's; with neither it is true (LRM 16.12.16).
struct PropertyCase {
  ExprId subject;
  std::vector<PropertyCaseItem> items;
  std::optional<PropertyExprId> default_property;

  auto operator==(const PropertyCase&) const -> bool = default;
};

using PropertyExprData = std::variant<
    PropertySequence, PropertyImplication, PropertyFollowedBy, PropertyNot,
    PropertyBinary, PropertyConditional, PropertyCase>;

struct PropertyExpr {
  PropertyExprData data;
  diag::SourceSpan span;

  auto operator==(const PropertyExpr&) const -> bool = default;
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

  auto operator==(const DisableCondition&) const -> bool = default;
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

  auto operator==(const PropertySpec&) const -> bool = default;
};

}  // namespace lyra::hir
