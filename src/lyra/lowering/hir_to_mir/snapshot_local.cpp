#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/verify.hpp"

namespace lyra::lowering::hir_to_mir {

auto SnapshotExprToLocal(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::Builder wrapper,
    mir::TypeId type, mir::ExprId expr_id) -> mir::LocalId {
  const mir::LocalId snap_var = frame.bindings->DeclareAnonymous(type);
  const mir::ExprId default_init =
      wrapper.Add(BuildDefaultValueExpr(unit_lowerer.Unit(), wrapper, type));
  wrapper.AppendStmt(
      mir::LocalDeclStmt{.target = snap_var, .init = default_init});

  const mir::ExprId target_id = wrapper.LocalPlace(snap_var, type);
  const mir::ExprId assign_id = wrapper.Add(
      mir::MakeAssignExpr(unit_lowerer.Unit().builtins, target_id, expr_id));
  wrapper.AppendStmt(mir::ExprStmt{.expr = assign_id});

  return snap_var;
}

auto DeclareLocal(const WalkFrame& frame, mir::ExprId init) -> mir::LocalId {
  mir::Builder block = frame.Builder();
  // A local starts as a value, so storage it is initialized from is read.
  if (mir::IsPlace(block.Get(init).data)) {
    init = block.Read(init);
  }
  const mir::LocalId local =
      frame.bindings->DeclareAnonymous(block.Get(init).type);
  block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = init});
  return local;
}

void AssignLocal(
    const mir::CompilationUnit& unit, mir::Builder block, mir::LocalId local,
    mir::ExprId value) {
  const mir::TypeId type = block.Get(value).type;
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(
              mir::MakeAssignExpr(
                  unit.builtins, block.LocalPlace(local, type), value))});
}

auto EvaluatedOnce(const WalkFrame& frame, mir::ExprId value) -> mir::ExprId {
  mir::Builder block = frame.Builder();
  if (mir::EvaluatesNothing(frame.bindings->Unit(), block.Body(), value)) {
    return value;
  }
  const mir::TypeId type = block.Get(value).type;
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
    return closure.Body().Add(outer);
  }
  return closure.Body().Read(
      SnapshotPlaceIntoClosure(unit_lowerer, outer_frame, closure, outer_expr));
}

auto SnapshotPlaceIntoClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr) -> mir::ExprId {
  const BodyBindingRef ref = SnapshotBindingIntoClosure(
      unit_lowerer, outer_frame, closure, outer_expr);
  return closure.Body().Add(
      closure.Bindings().MakeBindingPlace(ref, closure.Body()));
}

auto DeclareSynthesizedLocal(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId value)
    -> BindingOriginId {
  mir::Builder block = frame.Builder();
  // The site uniquely identifies this synthesized local, so several in one
  // body never collide on origin. The local starts as the value itself (not a
  // default then an assignment), so one of a type with no default value -- a
  // `VariableRef<T>` over the target cell -- is materialized correctly.
  const BindingOriginId origin = SynthesizedOrigin{
      .owner_site = unit_lowerer.NextSynthesizedSite(), .ordinal = 0};
  const mir::LocalId local =
      frame.bindings->Declare(origin, block.Get(value).type);
  block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = value});
  return origin;
}

auto SnapshotBindingIntoClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr) -> BodyBindingRef {
  return closure.Bindings().EnsureCarrier(
      DeclareSynthesizedLocal(unit_lowerer, outer_frame, outer_expr));
}

}  // namespace lyra::lowering::hir_to_mir
