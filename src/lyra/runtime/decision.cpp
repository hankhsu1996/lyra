#include "lyra/runtime/decision.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/reporting.hpp"

namespace lyra::runtime {

void Engine::RecordDecisionObservation(
    DecisionOwnerId owner_id, DecisionId decision_id, MatchClass match_class,
    DecisionSelectedKind selected_kind, DecisionArmIndex selected_arm) {
  if (owner_id.Index() >= decision_owner_states_.size()) {
    throw common::InternalError(
        "Engine::RecordDecisionObservation",
        std::format(
            "owner_id {} out of range (states size {})", owner_id.Index(),
            decision_owner_states_.size()));
  }
  if (owner_id.Index() >= decision_owner_pending_flags_.size()) {
    throw common::InternalError(
        "Engine::RecordDecisionObservation",
        std::format(
            "owner_id {} out of range (pending flags size {})",
            owner_id.Index(), decision_owner_pending_flags_.size()));
  }

  auto& state = decision_owner_states_[owner_id.Index()];
  if (decision_id.Index() >= state.slots.size()) {
    throw common::InternalError(
        "Engine::RecordDecisionObservation",
        std::format(
            "decision_id {} out of range for owner {} (slots size {})",
            decision_id.Index(), owner_id.Index(), state.slots.size()));
  }

  auto& slot = state.slots[decision_id.Index()];
  slot.timeslot_epoch = current_timeslot_epoch_;
  slot.match_class = match_class;
  slot.selected_kind = selected_kind;
  slot.selected_arm = selected_arm;

  if (state.dirty_seen[decision_id.Index()] == 0) {
    state.dirty_seen[decision_id.Index()] = 1;
    state.dirty_list.push_back(decision_id);
  }

  if (decision_owner_pending_flags_[owner_id.Index()] == 0) {
    decision_owner_pending_flags_[owner_id.Index()] = 1;
    pending_decision_owners_.push_back(owner_id);
  }
}

auto Engine::FormatDecisionOwner(DecisionOwnerId owner_id) const
    -> std::string {
  return FormatProcess(owner_id.Index());
}

auto Engine::BuildDecisionViolationMessage(
    DecisionOwnerId owner_id, const DecisionMetaEntry& meta,
    DecisionViolation violation) const -> std::string {
  auto kind = DecisionMetaKind(meta);
  bool is_case = kind == DecisionKind::kCase;

  const char* violation_str = [&]() {
    if (violation == DecisionViolation::kOverlap) {
      return is_case ? "multiple case items match"
                     : "multiple conditions matched";
    }
    return is_case ? "no matching case item" : "no condition matched";
  }();

  const char* qualifier_str = [&]() {
    switch (DecisionMetaQualifier(meta)) {
      case DecisionQualifier::kUnique:
        return "unique";
      case DecisionQualifier::kUnique0:
        return "unique0";
      case DecisionQualifier::kPriority:
        return "priority";
      default:
        return "";
    }
  }();

  const char* kind_str = is_case ? "case" : "if";
  std::string owner_str = FormatDecisionOwner(owner_id);
  return std::format(
      "{} {} violation in {}: {}", qualifier_str, kind_str, owner_str,
      violation_str);
}

auto Engine::BuildDecisionViolationReport(
    DecisionOwnerId owner_id, const DecisionMetaEntry& meta,
    DecisionViolation violation) const -> ReportRequest {
  std::optional<ResolvedOrigin> origin;
  if (meta.file != nullptr && meta.line > 0) {
    origin =
        ResolvedOrigin{.file = meta.file, .line = meta.line, .col = meta.col};
  }
  return ReportRequest{
      .kind = ReportKind::kDecisionViolation,
      .severity = Severity::kWarning,
      .origin = origin,
      .message = BuildDecisionViolationMessage(owner_id, meta, violation),
      .action = ReportAction::kContinue,
  };
}

void Engine::RunSettleCompleteChecks() {
  for (DecisionOwnerId owner_id : pending_decision_owners_) {
    if (owner_id.Index() >= decision_owner_states_.size()) {
      throw common::InternalError(
          "Engine::RunSettleCompleteChecks",
          std::format(
              "pending owner_id {} out of range (states size {})",
              owner_id.Index(), decision_owner_states_.size()));
    }
    if (owner_id.Index() >= decision_owner_pending_flags_.size()) {
      throw common::InternalError(
          "Engine::RunSettleCompleteChecks",
          std::format(
              "pending owner_id {} out of range (pending flags size {})",
              owner_id.Index(), decision_owner_pending_flags_.size()));
    }
    ValidateDecisionOwnerChecks(owner_id);
    decision_owner_pending_flags_[owner_id.Index()] = 0;
  }
  pending_decision_owners_.clear();
}

void Engine::ValidateDecisionOwnerChecks(DecisionOwnerId owner_id) {
  if (owner_id.Index() >= decision_owner_states_.size()) {
    throw common::InternalError(
        "Engine::ValidateDecisionOwnerChecks",
        std::format(
            "owner_id {} out of range (states size {})", owner_id.Index(),
            decision_owner_states_.size()));
  }
  if (owner_id.Index() >= decision_owner_tables_.size()) {
    throw common::InternalError(
        "Engine::ValidateDecisionOwnerChecks",
        std::format(
            "owner_id {} out of range (tables size {})", owner_id.Index(),
            decision_owner_tables_.size()));
  }
  auto& state = decision_owner_states_[owner_id.Index()];
  const auto& table = decision_owner_tables_[owner_id.Index()];
  auto metas = (table.metas != nullptr)
                   ? std::span(table.metas, table.count.Index())
                   : std::span<const DecisionMetaEntry>{};

  for (DecisionId did : state.dirty_list) {
    auto& obs = state.slots[did.Index()];

    if (!(obs.timeslot_epoch == current_timeslot_epoch_)) continue;

    if (did.Index() >= metas.size()) {
      throw common::InternalError(
          "Engine::ValidateDecisionOwnerChecks",
          std::format(
              "decision_id {} out of range for owner {} (meta count {})",
              did.Index(), owner_id.Index(), metas.size()));
    }
    const auto& meta = metas[did.Index()];

    auto violation = ClassifyDecisionViolation(meta, obs);
    if (violation != DecisionViolation::kNone) {
      DecisionDiagKey key{
          .owner_id = owner_id,
          .decision_id = did,
          .violation = violation,
      };
      auto& count = decision_diag_counts_[key];
      if (count < 10) {
        ++count;
        EmitReport(
            this, BuildDecisionViolationReport(owner_id, meta, violation));
        if (count == 10) {
          std::optional<ResolvedOrigin> suppression_origin;
          if (meta.file != nullptr && meta.line > 0) {
            suppression_origin = ResolvedOrigin{
                .file = meta.file, .line = meta.line, .col = meta.col};
          }
          EmitReport(
              this, ReportRequest{
                        .kind = ReportKind::kDecisionViolation,
                        .severity = Severity::kWarning,
                        .origin = suppression_origin,
                        .message = std::format(
                            "further decision violations suppressed in {}",
                            FormatDecisionOwner(owner_id)),
                        .action = ReportAction::kContinue,
                    });
        }
      }
    }

    state.dirty_seen[did.Index()] = 0;
  }
  state.dirty_list.clear();
}

}  // namespace lyra::runtime

// Runtime ABI: called from LLVM-generated code with explicit caller context.
// Raw integers only at this C boundary; converted to runtime-namespace strong
// types immediately.
extern "C" void LyraRecordDecisionObservation(
    void* engine_ptr, uint32_t owner_id_raw, uint32_t decision_id_raw,
    uint8_t match_class_raw, uint8_t selected_kind_raw,
    uint16_t selected_arm_raw) {
  using lyra::runtime::DecisionArmIndex;
  using lyra::runtime::DecisionId;
  using lyra::runtime::DecisionOwnerId;
  using lyra::runtime::DecisionSelectedKind;
  using lyra::runtime::Engine;
  using lyra::runtime::MatchClass;
  auto* engine = static_cast<Engine*>(engine_ptr);
  engine->RecordDecisionObservation(
      DecisionOwnerId::FromIndex(owner_id_raw),
      DecisionId::FromIndex(decision_id_raw),
      static_cast<MatchClass>(match_class_raw),
      static_cast<DecisionSelectedKind>(selected_kind_raw),
      DecisionArmIndex{selected_arm_raw});
}
