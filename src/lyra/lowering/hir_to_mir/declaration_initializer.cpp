#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"

#include <expected>
#include <utility>

#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto IntegratePendingStaticInitializer(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const WalkFrame& init_frame, const PendingStaticInitializer& pending)
    -> diag::Result<void> {
  auto& init_block = *init_frame.current_block;
  const mir::TypeId self_ptr_type = init_frame.current_class->self_pointer_type;

  mir::ExprId init_value{};
  if (pending.init_expr.has_value()) {
    auto init_or =
        process.LowerExpr(body.exprs.Get(*pending.init_expr), init_frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = init_block.exprs.Add(*std::move(init_or));
  } else {
    init_value = init_block.exprs.Add(BuildDefaultValueFromHir(
        process.Module(), init_frame, pending.hir_type));
  }

  const mir::ExprId target = init_block.exprs.Add(
      process.BuildStaticStorageAccess(init_frame, pending.placement));
  // Route through the observable cell protocol when the storage is
  // observable-wrapped (a static owned by a materialized procedural-storage
  // scope, so a cross-unit pointer slot lines up with the wrapped cell);
  // plain assignment otherwise (LRM 13.3.1).
  const mir::ExprId services_self_read =
      init_block.exprs.Add(MakeSelfRefExpr(init_frame, self_ptr_type));
  const mir::ExprId services_id = init_block.exprs.Add(
      mir::MakeServicesCallExpr(
          services_self_read, process.Module().Unit().builtins.services));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      process.Module().Unit(), init_block, services_id, target, init_value,
      std::nullopt, pending.storage_type,
      process.Module().Unit().builtins.void_type);
  const mir::ExprId assign = init_block.exprs.Add(assign_expr);
  init_block.AppendStmt(mir::ExprStmt{.expr = assign});
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
