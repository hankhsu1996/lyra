#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 10.3.2 (continuous assignment) and LRM 9.2.2.2.1 (always_comb) share a
// runtime mental model: re-evaluate the assignment whenever any RHS read
// changes. HIR keeps continuous assignment as a distinct scope-level node so
// source diagnostics retain provenance; at HIR -> MIR we materialise the
// runtime shape directly here as a coroutine body `forever { lhs = rhs;
// SensitivityWaitStmt(reads); }`, which the caller registers as a startup
// activation. The body executes once at t=0 (the natural fall-through of the
// eternal loop) before the first wait, matching LRM 9.2.2.2's "evaluate at
// time 0" requirement for inferred sensitivity.
auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, WalkFrame frame, std::string name,
    const hir::ContinuousAssign& src, std::optional<NetDriver> net_driver)
    -> diag::Result<mir::MethodDecl> {
  mir::CompilationUnit& unit = lowerer.Module().Unit();
  mir::CallableCode code;
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{
          .name = "self", .type = frame.current_class->self_pointer_type});
  const WalkFrame outer_frame = frame.WithBlock(&code.body)
                                    .WithBindings(&bindings)
                                    .WithCoroutineBody(true);

  mir::Block body_block;
  const WalkFrame body_frame = outer_frame.WithBlock(&body_block);

  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  auto rhs_or = lowerer.LowerExpr(hir_scope.exprs.Get(src.rhs), body_frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::TypeId assign_type = (*rhs_or).type;
  const mir::ExprId rhs_id = body_block.exprs.Add(*std::move(rhs_or));

  // Build `self.Services()` in the body so an observable LHS routes through
  // `Var<T>::Set` (or `Mutate` for a selector chain) and a net driver routes
  // through `Driver<T>::Update`. The body's `self` is the receiver binding
  // seeded above, reached through the same capture machinery a process body's
  // self read uses.
  const mir::ExprId body_self_ref = body_block.exprs.Add(
      MakeSelfRefExpr(body_frame, frame.current_class->self_pointer_type));
  const mir::ExprId body_services_id = body_block.exprs.Add(
      mir::MakeServicesCallExpr(
          body_self_ref, lowerer.Module().Unit().builtins.services));

  mir::ExprId effect_id{};
  if (net_driver.has_value()) {
    // A net target drives one of the net's driver slots: the body updates the
    // driver handle member with the evaluated right-hand side, and the net
    // re-resolves (LRM 6.5).
    const mir::ExprId driver_self = body_block.exprs.Add(
        MakeSelfRefExpr(body_frame, frame.current_class->self_pointer_type));
    const mir::ExprId driver_access = body_block.exprs.Add(
        mir::MakeMemberAccessExpr(
            driver_self, mir::MemberRef{.var = net_driver->driver_member},
            net_driver->driver_type));
    effect_id = body_block.exprs.Add(
        mir::MakeNetDriverUpdateCallExpr(
            driver_access, body_services_id, rhs_id,
            lowerer.Module().Unit().builtins.void_type));
  } else {
    auto lhs_or =
        lowerer.LowerLhsExpr(hir_scope.exprs.Get(src.lhs), body_frame);
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    const mir::ExprId lhs_id = body_block.exprs.Add(*std::move(lhs_or));
    const mir::Expr assign_expr = BuildObservableAssignExpr(
        lowerer.Module().Unit(), body_block, body_services_id, lhs_id, rhs_id,
        std::nullopt, assign_type, lowerer.Module().Unit().builtins.void_type);
    effect_id = body_block.exprs.Add(assign_expr);
  }
  body_block.AppendStmt(mir::ExprStmt{.expr = effect_id});

  body_block.AppendStmt(MakeSensitivityWaitStmt(
      body_block, body_frame, lowerer, src.sensitivity_list));

  const mir::BlockId body_scope_id =
      code.body.child_scopes.Add(std::move(body_block));
  code.body.AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  code.body.AppendStmt(
      mir::ReturnStmt{.value = std::nullopt, .is_coroutine_return = true});
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;
  return mir::MethodDecl{
      .name = std::move(name),
      .code = std::move(code),
      .overrides = std::nullopt,
      .visibility = mir::MethodVisibility::kInternal};
}

}  // namespace lyra::lowering::hir_to_mir
