#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerEventEdge(hir::EventEdge edge) -> mir::EventEdge {
  switch (edge) {
    case hir::EventEdge::kAnyChange:
      return mir::EventEdge::kAnyChange;
    case hir::EventEdge::kPosedge:
      return mir::EventEdge::kPosedge;
    case hir::EventEdge::kNegedge:
      return mir::EventEdge::kNegedge;
    case hir::EventEdge::kBothEdges:
      return mir::EventEdge::kBothEdges;
  }
  throw InternalError("LowerEventEdge: unknown hir::EventEdge value");
}

}  // namespace

auto BuildSensitivityWaitStmt(
    const StructuralScopeLoweringState& scope_state,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt {
  std::vector<mir::SensitivityRead> reads;
  reads.reserve(sensitivity_list.size());
  for (const auto& entry : sensitivity_list) {
    reads.push_back(
        mir::SensitivityRead{
            .ref = scope_state.TranslateStructuralVar(
                entry.ref.hops, entry.ref.var),
            .bit_range = entry.bit_range,
            .edge_kind = LowerEventEdge(entry.edge_kind)});
  }
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::SensitivityWaitStmt{.reads = std::move(reads)},
      .child_procedural_scopes = {}};
}

}  // namespace lyra::lowering::hir_to_mir
