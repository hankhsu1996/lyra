#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
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

auto MakeSensitivityWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt {
  auto& unit = lowerer.Module().Unit();
  std::vector<mir::SensitivityRead> reads;
  reads.reserve(sensitivity_list.size());
  for (const auto& entry : sensitivity_list) {
    const mir::ExprId observable_ptr_id = EndpointObservablePtr(
        target_block, frame, unit, BindEndpoint(lowerer, frame, entry.ref));
    // LRM 9.4.2 / 9.4.2.2 / 9.4.3: a bit-addressed footprint becomes
    // `(lsb, hi - lsb + 1)`; a whole-signal read (no footprint) is encoded
    // as width 0 (the any-change form at the runtime trigger).
    const std::uint64_t lsb_bit_offset =
        entry.footprint.has_value() ? entry.footprint->first : 0;
    const std::uint64_t bit_width =
        entry.footprint.has_value()
            ? entry.footprint->second - entry.footprint->first + 1
            : 0;
    reads.push_back(
        mir::SensitivityRead{
            .observable_ptr = observable_ptr_id,
            .lsb_bit_offset = lsb_bit_offset,
            .bit_width = bit_width,
            .edge_kind = LowerEventEdge(entry.edge_kind)});
  }
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::SensitivityWaitStmt{.reads = std::move(reads)}};
}

}  // namespace lyra::lowering::hir_to_mir
