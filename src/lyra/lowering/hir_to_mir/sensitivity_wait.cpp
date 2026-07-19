#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// One leaf of the wait: the observable cell it watches, the bit projection of
// that cell's packed encoding it watches, and the edge polarity it watches for.
// LRM 9.4.2 / 9.4.2.2 / 9.4.3: a bit-addressed footprint becomes
// `(lsb, hi - lsb + 1)`; a whole-signal read (no footprint) is width 0, which
// the runtime reads as the whole cell.
auto BuildTriggerExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    const StructuralScopeLowerer& lowerer, const hir::SensitivityEntry& entry)
    -> mir::ExprId {
  // The leaf watches either an intra-unit cell reached through its route, or a
  // package variable's one program-global cell reached by name (LRM 26.2). Both
  // resolve to a borrowed pointer to the observable cell the runtime subscribes
  // to; only the way the cell is reached differs.
  const mir::ExprId observable_ptr = std::visit(
      Overloaded{
          [&](const hir::ReferenceRoute& route) -> mir::ExprId {
            return EndpointObservablePtr(
                block, frame, unit, BindEndpoint(lowerer, frame, route));
          },
          [&](const hir::ExternalUnitValueRef& pkg) -> mir::ExprId {
            unit.AddExternalReferencedUnit(pkg.unit_name);
            const mir::TypeId cell_type = unit.types.ObservableCellOf(
                lowerer.Owner().TranslateType(pkg.value_type));
            const mir::ExprId cell = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::ExternalUnitVariableRef{
                            .unit_name = pkg.unit_name,
                            .variable_name = pkg.variable_name},
                    .type = cell_type});
            const mir::TypeId ptr_type = unit.types.PointerTo(
                cell_type, mir::PointerOwnership::kBorrowed);
            return block.exprs.Add(mir::MakeAddressOfExpr(cell, ptr_type));
          },
      },
      entry.ref);
  const std::int64_t lsb_bit_offset =
      entry.footprint.has_value()
          ? static_cast<std::int64_t>(entry.footprint->first)
          : 0;
  const std::int64_t bit_width =
      entry.footprint.has_value()
          ? static_cast<std::int64_t>(
                entry.footprint->second - entry.footprint->first + 1)
          : 0;
  const auto int_literal = [&](std::int64_t value) {
    return block.exprs.Add(mir::MakeIntLiteral(unit.builtins.int_type, value));
  };
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments =
                      {observable_ptr,
                       int_literal(static_cast<std::int64_t>(entry.edge_kind)),
                       int_literal(lsb_bit_offset), int_literal(bit_width)}},
          .type = unit.builtins.trigger});
}

}  // namespace

auto MakeValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt {
  auto& unit = lowerer.Owner().Unit();

  std::vector<mir::ExprId> triggers;
  triggers.reserve(sensitivity_list.size());
  for (const auto& entry : sensitivity_list) {
    triggers.push_back(
        BuildTriggerExpr(target_block, frame, unit, lowerer, entry));
  }
  const mir::TypeId triggers_type = unit.types.Intern(
      mir::UnpackedArrayType{
          .element_type = unit.builtins.trigger,
          .dim = mir::UnpackedRange::ZeroBased(triggers.size())});
  const mir::ExprId triggers_id = target_block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(triggers)},
          .type = triggers_type});

  const mir::ExprId services_id =
      target_block.exprs.Add(BuildServicesCallExpr(lowerer.Owner(), frame));
  const mir::ExprId call_id = target_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = support::BuiltinFn::kWaitAny},
                  .arguments = {services_id, triggers_id}},
          .type = unit.builtins.void_type});
  const mir::ExprId await_id = target_block.exprs.Add(
      mir::Expr{
          .data = mir::AwaitExpr{.awaitable = call_id},
          .type = unit.builtins.void_type});

  return mir::Stmt{
      .label = std::nullopt, .data = mir::ExprStmt{.expr = await_id}};
}

}  // namespace lyra::lowering::hir_to_mir
