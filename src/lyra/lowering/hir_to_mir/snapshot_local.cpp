#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"

#include <cstdint>
#include <optional>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/verify.hpp"

namespace lyra::lowering::hir_to_mir {

auto SnapshotExprToLocal(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::Block& wrapper,
    mir::TypeId type, mir::ExprId expr_id,
    std::optional<BindingOriginId> origin) -> mir::LocalId {
  const mir::LocalId snap_var = origin.has_value()
                                    ? frame.bindings->Declare(*origin, type)
                                    : frame.bindings->DeclareAnonymous(type);
  const mir::ExprId default_init = wrapper.exprs.Add(
      BuildDefaultValueExpr(unit_lowerer.Unit(), wrapper, type));
  wrapper.AppendStmt(
      mir::LocalDeclStmt{.target = snap_var, .init = default_init});

  const mir::ExprId target_id = wrapper.LocalPlace(snap_var, type);
  const mir::ExprId assign_id = wrapper.exprs.Add(
      mir::MakeAssignExpr(unit_lowerer.Unit().builtins, target_id, expr_id));
  wrapper.AppendStmt(mir::ExprStmt{.expr = assign_id});

  return snap_var;
}

auto DeclareLocal(const WalkFrame& frame, mir::ExprId init) -> mir::LocalId {
  mir::Block& block = *frame.current_block;
  // A local starts as a value, so storage it is initialized from is read.
  if (mir::IsPlace(block.exprs.Get(init).data)) {
    init = block.Read(init);
  }
  const mir::LocalId local =
      frame.bindings->DeclareAnonymous(block.exprs.Get(init).type);
  block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = init});
  return local;
}

void AssignLocal(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId local,
    mir::ExprId value) {
  const mir::TypeId type = block.exprs.Get(value).type;
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins, block.LocalPlace(local, type), value))});
}

auto EvaluatedOnce(const WalkFrame& frame, mir::ExprId value) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  if (mir::EvaluatesNothing(block, value)) {
    return value;
  }
  const mir::TypeId type = block.exprs.Get(value).type;
  return block.ReadLocal(DeclareLocal(frame, value), type);
}

auto SnapshotValueIntoClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr) -> mir::ExprId {
  // A constant is the same value whenever the body runs, so there is nothing
  // to freeze and the body states it itself, as a C++ lambda captures no
  // variable usable in constant expressions ([basic.def.odr]).
  const mir::Expr& outer = outer_frame.current_block->exprs.Get(outer_expr);
  if (mir::IsConstant(outer.data)) {
    return closure.Body().exprs.Add(outer);
  }
  return closure.Body().Read(
      SnapshotPlaceIntoClosure(unit_lowerer, outer_frame, closure, outer_expr));
}

auto SnapshotPlaceIntoClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr) -> mir::ExprId {
  const BodyBindingRef ref = SnapshotBindingIntoClosure(
      unit_lowerer, outer_frame, closure, outer_expr);
  return closure.Body().exprs.Add(
      closure.Bindings().MakeBindingPlace(ref, closure.Body()));
}

auto SnapshotBindingIntoClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr) -> BodyBindingRef {
  mir::Block& outer_block = *outer_frame.current_block;
  const mir::TypeId type = outer_block.exprs.Get(outer_expr).type;

  // The site uniquely identifies this synthesized carrier, so several snapshots
  // in one outer body never collide on origin. The temp initializes directly
  // from the snapshotted expression (not a default-then-assign), so a carrier
  // with no default value -- a `VariableRef<T>` over the target cell -- is
  // materialized correctly.
  const std::uint32_t site = unit_lowerer.NextSynthesizedSite();
  const BindingOriginId origin =
      SynthesizedOrigin{.owner_site = site, .ordinal = 0};
  const mir::LocalId temp = outer_frame.bindings->Declare(origin, type);
  outer_block.AppendStmt(
      mir::LocalDeclStmt{.target = temp, .init = outer_expr});

  return closure.Bindings().EnsureCarrier(origin);
}

}  // namespace lyra::lowering::hir_to_mir
