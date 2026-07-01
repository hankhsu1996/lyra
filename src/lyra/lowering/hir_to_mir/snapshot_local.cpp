#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"

#include <cstdint>
#include <optional>
#include <string>
#include <utility>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto SnapshotExprToLocal(
    const ModuleLowerer& module, WalkFrame frame, mir::Block& wrapper,
    std::string name, mir::TypeId type, mir::ExprId expr_id,
    std::optional<BindingOriginId> origin) -> mir::LocalId {
  const mir::LocalId snap_var =
      origin.has_value()
          ? frame.bindings->Declare(
                *origin, mir::LocalDecl{.name = std::move(name), .type = type})
          : frame.bindings->DeclareAnonymous(
                mir::LocalDecl{.name = std::move(name), .type = type});
  const mir::ExprId default_init =
      wrapper.exprs.Add(BuildDefaultValueExpr(module, frame, type));
  wrapper.AppendStmt(
      mir::LocalDeclStmt{.target = snap_var, .init = default_init});

  const mir::ExprId target_id =
      wrapper.exprs.Add(mir::MakeLocalRefExpr(snap_var, type));
  const mir::ExprId assign_id = wrapper.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = target_id, .value = expr_id},
          .type = type});
  wrapper.AppendStmt(mir::ExprStmt{.expr = assign_id});

  return snap_var;
}

auto SnapshotIntoClosure(
    ModuleLowerer& module, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr, std::string name)
    -> mir::ExprId {
  mir::Block& outer_block = *outer_frame.current_block;
  const mir::TypeId type = outer_block.exprs.Get(outer_expr).type;

  // The site uniquely identifies this synthesized carrier and tags its name, so
  // several snapshots in one outer body never collide on origin or C++ name.
  // The temp initializes directly from the snapshotted expression (not a
  // default-then-assign), so a carrier with no default value -- a `Ref<T>` over
  // the target cell -- is materialized correctly.
  const std::uint32_t site = module.NextSynthesizedSite();
  const BindingOriginId origin = BindingOriginId::Synthesized(site, 0);
  const mir::LocalId temp = outer_frame.bindings->Declare(
      origin,
      mir::LocalDecl{
          .name = std::move(name) + "_" + std::to_string(site), .type = type});
  outer_block.AppendStmt(
      mir::LocalDeclStmt{.target = temp, .init = outer_expr});

  const BodyBindingRef ref = closure.Bindings().EnsureCarrier(origin);
  return closure.Body().exprs.Add(
      closure.Bindings().MakeReadExpr(ref, closure.Body()));
}

}  // namespace lyra::lowering::hir_to_mir
