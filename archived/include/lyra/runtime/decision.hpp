#pragma once

#include <cstdint>
#include <functional>
#include <vector>

#include "lyra/semantic/decision.hpp"

namespace lyra::runtime {

using semantic::DecisionArmCount;
using semantic::DecisionArmIndex;
using semantic::DecisionId;
using semantic::DecisionKind;
using semantic::DecisionOwnerId;
using semantic::DecisionQualifier;
using semantic::DecisionSelectedKind;
using semantic::DecisionSiteCount;
using semantic::DecisionViolation;
using semantic::MatchClass;
using semantic::OptionalDecisionOwnerId;
using semantic::TimeslotEpoch;

// Immutable per-site metadata. Emitted once per process body as an LLVM
// global. Binary contract with codegen (EmitDecisionMetaTable).
// All fields are compile-time constants derived from MirDecisionSite.
struct DecisionMetaEntry {
  // qualifier(8) | kind(8) | has_fallback(8) | reserved(8)
  uint32_t qualifier_kind_packed = 0;
  uint32_t arm_count = 0;
  // Source file path. Points to LLVM global string; valid for program lifetime.
  // Null when source location is unknown.
  const char* file = nullptr;
  uint32_t line = 0;
  uint32_t col = 0;
};

// Convenience accessors for packed fields.
inline auto DecisionMetaQualifier(const DecisionMetaEntry& e)
    -> DecisionQualifier {
  return static_cast<DecisionQualifier>(e.qualifier_kind_packed & 0xFF);
}
inline auto DecisionMetaKind(const DecisionMetaEntry& e) -> DecisionKind {
  return static_cast<DecisionKind>((e.qualifier_kind_packed >> 8) & 0xFF);
}
inline auto DecisionMetaHasFallback(const DecisionMetaEntry& e) -> bool {
  return ((e.qualifier_kind_packed >> 16) & 0xFF) != 0;
}

// Per-body decision table descriptor. One per body-local process.
// Emitted as an LLVM global and passed through the body descriptor path.
struct DecisionTableDescriptor {
  const DecisionMetaEntry* metas;
  uint32_t count;
};

// Per-owner immutable table pointer. Stored on Engine, indexed by owner_id.
// Points into emitted LLVM globals; valid for the lifetime of the program.
struct DecisionOwnerTable {
  DecisionSiteCount count;
  const DecisionMetaEntry* metas = nullptr;
};

// Per-site mutable observation. Last-write-wins within a timeslot.
// Contains ONLY the observation; metadata is in the immutable table.
struct DecisionObservation {
  TimeslotEpoch timeslot_epoch;
  MatchClass match_class{};
  DecisionSelectedKind selected_kind{};
  DecisionArmIndex selected_arm;
};

// Per-owner mutable decision state. Sized from the immutable table count
// during registration, not lazily.
struct DecisionOwnerState {
  std::vector<DecisionObservation> slots;
  std::vector<uint8_t> dirty_seen;
  std::vector<DecisionId> dirty_list;
};

// Classify a settled observation against its immutable metadata.
inline auto ClassifyDecisionViolation(
    const DecisionMetaEntry& meta, const DecisionObservation& obs)
    -> DecisionViolation {
  auto qualifier = DecisionMetaQualifier(meta);
  bool has_fallback = DecisionMetaHasFallback(meta);
  if (semantic::NeedsOverlapCheck(qualifier) &&
      obs.match_class == MatchClass::kMultiple) {
    return DecisionViolation::kOverlap;
  }
  if (semantic::NeedsNoMatchCheck(qualifier, has_fallback) &&
      obs.match_class == MatchClass::kZero) {
    return DecisionViolation::kNoMatch;
  }
  return DecisionViolation::kNone;
}

// Runtime process identity. Wraps the flat process_id index used by the
// engine's per-process arrays. Raw uint32_t only at extern "C" boundaries.
struct ProcessId {
  uint32_t raw = 0;

  static auto FromIndex(uint32_t v) -> ProcessId {
    return ProcessId{v};
  }
  [[nodiscard]] auto Index() const -> uint32_t {
    return raw;
  }

  friend auto operator==(ProcessId, ProcessId) -> bool = default;
};

// Rate-limiting key for decision diagnostics. Keyed by (owner, site,
// violation) so overlap and no-match share separate rate-limit buckets.
struct DecisionDiagKey {
  DecisionOwnerId owner_id;
  DecisionId decision_id;
  DecisionViolation violation{};

  auto operator==(const DecisionDiagKey&) const -> bool = default;
};

struct DecisionDiagKeyHash {
  auto operator()(const DecisionDiagKey& k) const -> size_t {
    size_t h = std::hash<uint32_t>{}(k.owner_id.Index());
    h ^= std::hash<uint32_t>{}(k.decision_id.Index()) + 0x9e3779b9 + (h << 6) +
         (h >> 2);
    h ^= std::hash<uint32_t>{}(static_cast<uint32_t>(k.violation)) +
         0x9e3779b9 + (h << 6) + (h >> 2);
    return h;
  }
};

}  // namespace lyra::runtime
