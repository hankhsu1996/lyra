#pragma once

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/common/slot_id.hpp"

namespace lyra::lowering::mir_to_llvm {

struct ConnectionIndex {
  uint32_t value = 0;
  auto operator==(const ConnectionIndex&) const -> bool = default;
};

// Port-binding forwarding candidate. Analysis-only result; does NOT
// authorize transformation. Each field records the result of one
// candidate check for reviewability. Unresolved checks are marked
// explicitly.
struct PortBindingForwardingCandidate {
  common::SlotId intermediate_slot_id{};
  ConnectionIndex upstream_connection_index{};
  ConnectionIndex downstream_connection_index{};

  // Provably checked conditions.
  bool single_writer = false;
  bool single_downstream = false;
  bool both_port_binding = false;
  bool no_process_trigger = false;
  bool no_comb_trigger = false;

  // Shape checks -- necessary but not sufficient for full proof.
  bool upstream_full_copy_shape = false;
  bool downstream_matching_read_shape = false;

  // Whether trace/display/strobe references could not be resolved at
  // compile time. When true, the candidate is not transform-safe.
  bool trace_ref_unresolved = true;

  [[nodiscard]] auto IsProvablyPassingCurrentChecks() const -> bool {
    return single_writer && single_downstream && both_port_binding &&
           no_process_trigger && no_comb_trigger && upstream_full_copy_shape &&
           downstream_matching_read_shape;
  }
};

// Aggregated forwarding analysis report. Owns the candidate list and
// provides summary queries so renderers stay purely presentational.
class ForwardingAnalysisReport {
 public:
  ForwardingAnalysisReport() = default;

  explicit ForwardingAnalysisReport(
      std::vector<PortBindingForwardingCandidate> candidates)
      : candidates_(std::move(candidates)) {
  }

  [[nodiscard]] auto Empty() const -> bool {
    return candidates_.empty();
  }

  [[nodiscard]] auto Candidates() const
      -> std::span<const PortBindingForwardingCandidate> {
    return candidates_;
  }

  [[nodiscard]] auto ProvablePassCount() const -> uint32_t {
    uint32_t count = 0;
    for (const auto& c : candidates_) {
      if (c.IsProvablyPassingCurrentChecks()) ++count;
    }
    return count;
  }

  [[nodiscard]] auto AllTraceRefsUnresolved() const -> bool {
    return std::all_of(
        candidates_.begin(), candidates_.end(),
        [](const auto& c) { return c.trace_ref_unresolved; });
  }

  void AddCandidate(PortBindingForwardingCandidate c) {
    candidates_.push_back(std::move(c));
  }

 private:
  std::vector<PortBindingForwardingCandidate> candidates_;
};

struct LoweringReport {
  ForwardingAnalysisReport forwarding_analysis;
  uint32_t relay_slots_eliminated = 0;
};

}  // namespace lyra::lowering::mir_to_llvm
