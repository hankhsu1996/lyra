#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <cstdint>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/value_ref.hpp"
#include "lyra/support/builtin_fn.hpp"

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

// Selects the observable-pointer expression for one sensitivity leaf, given
// the lowered MIR member ref and its slot type. Adds the chosen expression
// (and any sub-expressions it needs) to `block` and returns its id. The
// three cases mirror the runtime's three storage shapes for an observable:
// upward-ref slot (needs `AsObservable` resolution), pre-resolved borrowed
// pointer (already an observable*), and a directly owned cell (address-of).
auto BuildObservablePtrExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    mir::EnclosingHops hops, mir::MemberId var) -> mir::ExprId {
  const mir::TypeId field_type =
      frame.EnclosingClassAtHops(hops).members.Get(var).type;
  const mir::ExprId member_access_id =
      block.exprs.Add(BuildStructuralMemberAccessExpr(frame, unit, hops, var));
  const auto& field_type_data = unit.types.Get(field_type).data;

  if (std::holds_alternative<mir::ExternalRefType>(field_type_data)) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kAsObservable},
                    .arguments = {member_access_id}},
            .type = field_type});
  }

  if (const auto* ptr = std::get_if<mir::PointerType>(&field_type_data);
      ptr != nullptr && ptr->ownership == mir::PointerOwnership::kBorrowed) {
    return member_access_id;
  }

  const mir::TypeId ptr_type =
      unit.types.PointerTo(field_type, mir::PointerOwnership::kBorrowed);
  return block.exprs.Add(mir::MakeAddressOfExpr(member_access_id, ptr_type));
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
    const auto [hops, var] = std::visit(
        Overloaded{
            [&](const hir::StructuralDataObjectRef& r)
                -> std::pair<mir::EnclosingHops, mir::MemberId> {
              return {
                  mir::EnclosingHops{.value = r.hops.value},
                  lowerer.TranslateStructuralDataObject(r.hops, r.var)};
            },
            [&](const hir::CrossUnitVarRef& r)
                -> std::pair<mir::EnclosingHops, mir::MemberId> {
              return {
                  mir::EnclosingHops{.value = 0},
                  lowerer.CrossUnitRefTarget(r.id).target.var};
            },
        },
        entry.ref);
    const mir::ExprId observable_ptr_id =
        BuildObservablePtrExpr(target_block, frame, unit, hops, var);
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
