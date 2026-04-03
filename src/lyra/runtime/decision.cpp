#include "lyra/runtime/decision.hpp"

#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <string_view>

#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/output_sink.hpp"

namespace lyra::runtime {

void Engine::RecordDecisionObservation(
    ProcessId process_id, DecisionId decision_id, MatchClass match_class,
    DecisionSelectedKind selected_kind, DecisionArmIndex selected_arm) {
  if (process_id.Index() >= decision_states_.size()) return;

  auto& state = decision_states_[process_id.Index()];
  if (decision_id.Index() >= state.slots.size()) return;

  auto& slot = state.slots[decision_id.Index()];
  slot.timeslot_epoch = current_timeslot_epoch_;
  slot.match_class = match_class;
  slot.selected_kind = selected_kind;
  slot.selected_arm = selected_arm;

  if (state.dirty_seen[decision_id.Index()] == 0) {
    state.dirty_seen[decision_id.Index()] = 1;
    state.dirty_list.push_back(decision_id);
  }

  if (decision_pending_flags_[process_id.Index()] == 0) {
    decision_pending_flags_[process_id.Index()] = 1;
    pending_decision_processes_.push_back(process_id);
  }
}

auto Engine::FormatDecisionViolation(
    ProcessId process_id, const DecisionMetaEntry& meta,
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
  std::string proc_str = FormatProcess(process_id.Index());
  std::string_view file = (meta.file != nullptr) ? meta.file : "";

  if (meta.line > 0 && !file.empty()) {
    return std::format(
        "{}:{}:{}: warning: {} {} violation in {}: {}\n", file, meta.line,
        meta.col, qualifier_str, kind_str, proc_str, violation_str);
  }
  if (meta.line > 0) {
    return std::format(
        "warning: {} {} violation at line {} in {}: {}\n", qualifier_str,
        kind_str, meta.line, proc_str, violation_str);
  }
  return std::format(
      "warning: {} {} violation in {}: {}\n", qualifier_str, kind_str, proc_str,
      violation_str);
}

void Engine::RunSettleCompleteChecks() {
  for (ProcessId pid : pending_decision_processes_) {
    ValidateProcessDecisionChecks(pid);
    decision_pending_flags_[pid.Index()] = 0;
  }
  pending_decision_processes_.clear();
}

void Engine::ValidateProcessDecisionChecks(ProcessId process_id) {
  auto& state = decision_states_[process_id.Index()];
  const auto& table = process_decision_tables_[process_id.Index()];
  auto metas = (table.metas != nullptr)
                   ? std::span(table.metas, table.count.Index())
                   : std::span<const DecisionMetaEntry>{};

  for (DecisionId did : state.dirty_list) {
    auto& obs = state.slots[did.Index()];

    if (!(obs.timeslot_epoch == current_timeslot_epoch_)) continue;

    if (did.Index() >= metas.size()) continue;
    const auto& meta = metas[did.Index()];

    auto violation = ClassifyDecisionViolation(meta, obs);
    if (violation != DecisionViolation::kNone) {
      DecisionDiagKey key{
          .process_id = process_id,
          .decision_id = did,
          .violation = violation,
      };
      auto& count = decision_diag_counts_[key];
      if (count < 10) {
        ++count;
        WriteOutput(FormatDecisionViolation(process_id, meta, violation));
        if (count == 10) {
          std::string proc_str = FormatProcess(process_id.Index());
          WriteOutput(
              std::format(
                  "  (further violations in {} suppressed)\n", proc_str));
        }
      }
    }

    state.dirty_seen[did.Index()] = 0;
  }
  state.dirty_list.clear();
}

}  // namespace lyra::runtime

// Runtime ABI: called from LLVM-generated code with explicit caller context.
// Raw integers only at this C boundary; converted to strong types immediately.
extern "C" void LyraRecordDecisionObservation(
    void* engine_ptr, uint32_t process_id_raw, uint32_t decision_id_raw,
    uint8_t match_class_raw, uint8_t selected_kind_raw,
    uint16_t selected_arm_raw) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->RecordDecisionObservation(
      lyra::runtime::ProcessId::FromIndex(process_id_raw),
      lyra::semantic::DecisionId::FromIndex(decision_id_raw),
      static_cast<lyra::semantic::MatchClass>(match_class_raw),
      static_cast<lyra::semantic::DecisionSelectedKind>(selected_kind_raw),
      lyra::semantic::DecisionArmIndex{selected_arm_raw});
}
