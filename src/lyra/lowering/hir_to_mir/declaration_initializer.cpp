#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"

#include <expected>
#include <utility>

#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto IntegratePendingStaticInitializer(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const WalkFrame& init_frame, const PendingStaticInitializer& pending)
    -> diag::Result<void> {
  auto& init_block = *init_frame.current_block;
  const mir::CompilationUnit& unit = process.Owner().Unit();

  const mir::ExprId target = init_block.exprs.Add(
      process.BuildStaticStorageAccess(init_frame, pending.placement));
  const bool target_is_observable_cell = mir::IsObservableCellType(
      unit.types.Get(init_block.exprs.Get(target).type));

  // An observable cell installs its declared representation and default
  // contents once at construction (LRM 10.5); a later user initializer stores
  // through Set, which verifies the value against the installed
  // representation. The default-only case is fully expressed by Initialize
  // and needs no Set.
  if (target_is_observable_cell) {
    const mir::ExprId prototype = init_block.exprs.Add(BuildDefaultValueFromHir(
        process.Owner(), init_frame, pending.hir_type));
    init_block.AppendStmt(
        mir::ExprStmt{
            .expr = init_block.exprs.Add(
                mir::MakeObservableInitializeCallExpr(
                    target, prototype, unit.builtins.void_type))});
    if (!pending.init_expr.has_value()) {
      return {};
    }
  }

  mir::ExprId init_value{};
  if (pending.init_expr.has_value()) {
    auto init_or =
        process.LowerExpr(body.exprs.Get(*pending.init_expr), init_frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = init_block.exprs.Add(*std::move(init_or));
  } else {
    init_value = init_block.exprs.Add(BuildDefaultValueFromHir(
        process.Owner(), init_frame, pending.hir_type));
  }

  const mir::ExprId runtime_id = init_block.exprs.Add(
      mir::MakeCurrentRuntimeCallExpr(unit.builtins.effects));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      unit, init_block, runtime_id, target, init_value, std::nullopt,
      pending.storage_type, unit.builtins.void_type);
  init_block.AppendStmt(
      mir::ExprStmt{.expr = init_block.exprs.Add(assign_expr)});
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
