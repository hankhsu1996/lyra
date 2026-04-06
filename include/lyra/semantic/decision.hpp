#pragma once

#include <cstdint>

namespace lyra::semantic {

// Decision qualifier for if/case statements (LRM 12.4.2, 12.5.3).
// Shared across MIR and runtime -- one semantic domain, one type.
enum class DecisionQualifier : uint8_t {
  kNone,
  kUnique,
  kUnique0,
  kPriority,
};

// Source statement kind for decision diagnostics.
enum class DecisionKind : uint8_t {
  kIf,
  kCase,
};

// Saturated match count classification.
// Validation only needs: zero / exactly one / more than one.
enum class MatchClass : uint8_t {
  kZero,
  kOne,
  kMultiple,
};

// Validation violation kind.
enum class DecisionViolation : uint8_t {
  kNone,
  kOverlap,
  kNoMatch,
};

// How the decision was resolved at runtime.
enum class DecisionSelectedKind : uint8_t {
  kArm,       // Dispatched to a real condition arm
  kFallback,  // Dispatched to real else/default body
  kNoMatch,   // No condition matched, no else/default exists
};

// Body-local decision site identity.
// Assigned during MIR construction, stable for the lifetime of the compiled
// artifact. Indexes per-process decision metadata and per-frame observation
// slots.
struct DecisionId {
  uint32_t raw = 0;

  static auto FromIndex(uint32_t v) -> DecisionId {
    return DecisionId{v};
  }
  [[nodiscard]] auto Index() const -> uint32_t {
    return raw;
  }

  friend auto operator==(DecisionId, DecisionId) -> bool = default;
};

// Arm index within one decision site.
// Valid only when DecisionSelectedKind == kArm.
struct DecisionArmIndex {
  uint16_t raw = 0;

  [[nodiscard]] auto Index() const -> uint16_t {
    return raw;
  }

  friend auto operator==(DecisionArmIndex, DecisionArmIndex) -> bool = default;
};

// Number of condition arms in a decision site (excludes fallback).
struct DecisionArmCount {
  uint16_t raw = 0;

  friend auto operator==(DecisionArmCount, DecisionArmCount) -> bool = default;
};

// Number of decision sites in a process body.
struct DecisionSiteCount {
  uint32_t raw = 0;

  static auto FromCount(uint32_t v) -> DecisionSiteCount {
    return DecisionSiteCount{v};
  }
  [[nodiscard]] auto Index() const -> uint32_t {
    return raw;
  }

  friend auto operator==(DecisionSiteCount, DecisionSiteCount)
      -> bool = default;
};

// Monotonic timeslot epoch for observation staleness detection.
struct TimeslotEpoch {
  uint32_t raw = 0;

  [[nodiscard]] auto Next() const -> TimeslotEpoch {
    return TimeslotEpoch{raw + 1};
  }

  friend auto operator==(TimeslotEpoch, TimeslotEpoch) -> bool = default;
};

// Validation predicates.
inline auto NeedsOverlapCheck(DecisionQualifier q) -> bool {
  return q == DecisionQualifier::kUnique || q == DecisionQualifier::kUnique0;
}

inline auto NeedsNoMatchCheck(DecisionQualifier q, bool has_fallback) -> bool {
  if (has_fallback) return false;
  return q == DecisionQualifier::kUnique || q == DecisionQualifier::kPriority;
}

// Deferred-check owner identity. Assigned during design realization (1:1 with
// ProcessId in the first cut). Indexes engine decision storage and pending
// queues. Distinct from ProcessId to decouple the decision pipeline from the
// scheduler's process model.
struct DecisionOwnerId {
  uint32_t raw = 0;

  static auto FromIndex(uint32_t v) -> DecisionOwnerId {
    return DecisionOwnerId{v};
  }
  [[nodiscard]] auto Index() const -> uint32_t {
    return raw;
  }

  friend auto operator==(DecisionOwnerId, DecisionOwnerId) -> bool = default;
};

// Optional decision owner. Used in runtime structs where owner presence is
// conditional (e.g., DPI export call context).
struct OptionalDecisionOwnerId {
  DecisionOwnerId value;
  bool has_value = false;
};

}  // namespace lyra::semantic
