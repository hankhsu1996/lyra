#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"

#include <algorithm>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The MIR value type behind a resolved-net LHS, paired with the LHS target's
// stable identity. Only a bare direct or routed reference may name a net
// target: a select or other computed LHS is a variable path even if its root
// sits on a net (whose runtime protocol still forbids a direct write).
struct ResolvedNetLhs {
  mir::TypeId value_type;
  ContinuousWriteTarget target;
};

// Answers "is this LHS a resolved-net cell" at the type level, without
// lowering the LHS. Reads the target's own field type for a direct member, or
// the pointee of the pre-declared endpoint slot for a routed reference. In
// both cases, net-versus-variable is a property of the target's MIR type
// (LRM 6.5).
auto ClassifyLhsAsResolvedNet(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    hir::ExprId lhs_id) -> std::optional<ResolvedNetLhs> {
  const mir::CompilationUnit& unit = lowerer.Module().Unit();
  const auto* prim =
      std::get_if<hir::PrimaryExpr>(&lowerer.HirScope().exprs.Get(lhs_id).data);
  if (prim == nullptr) return std::nullopt;
  if (const auto* ref = std::get_if<hir::DirectMemberRef>(&prim->data)) {
    const BoundEndpoint endpoint =
        BindEndpoint(lowerer, frame, hir::ReferenceRoute{*ref});
    const auto* resolved = std::get_if<mir::ResolvedType>(
        &unit.types.Get(endpoint.cell_type).data);
    if (resolved == nullptr) return std::nullopt;
    return ResolvedNetLhs{.value_type = resolved->value, .target = ref->var};
  }
  if (const auto* rr = std::get_if<hir::RoutedRef>(&prim->data)) {
    const BoundEndpoint endpoint =
        BindEndpoint(lowerer, frame, hir::ReferenceRoute{*rr});
    const auto* resolved = std::get_if<mir::ResolvedType>(
        &unit.types.Get(endpoint.cell_type).data);
    if (resolved == nullptr) return std::nullopt;
    return ResolvedNetLhs{.value_type = resolved->value, .target = rr->id};
  }
  return std::nullopt;
}

// The runtime handle a body update writes into for a resolved-net target.
// A field on the enclosing class holds the handle; the field is attached
// at Resolve and seeded at Initialize.
struct AttachedDriver {
  mir::FieldId driver_field;
  mir::TypeId driver_type;
};

// Installs a driver on a net cell reached through a route: a driver-handle
// member bound at Resolve (`self->driver = net_access.AttachDriver()`) and
// seeded at Initialize (`self->driver.Update(services, source)`). The
// caller supplies `net_access` -- the resolve-phase lvalue of the net cell,
// itself the LHS lowered in `resolve_frame` -- so a same-scope target reads
// as `self->net`, and an enclosing or cross-unit target as a dereference of
// its resolve-filled endpoint slot. The driver handle field is added to the
// enclosing class under `driver_name` (LRM 6.5).
auto AttachDriver(
    const StructuralScopeLowerer& lowerer, mir::ExprId net_access,
    mir::TypeId value_type, const std::string& driver_name, hir::ExprId source,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame)
    -> diag::Result<AttachedDriver> {
  mir::CompilationUnit& unit = lowerer.Module().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  mir::Class& mir_class = *resolve_frame.current_class;
  const mir::TypeId self_ptr_type = mir_class.self_pointer_type;
  mir::Block& resolve_block = *resolve_frame.current_block;
  mir::Block& init_block = *init_frame.current_block;

  const mir::TypeId driver_type =
      unit.types.Intern(mir::DriverType{.value = value_type});
  const mir::FieldId driver_field = mir_class.fields.Add(
      mir::FieldDecl{.name = driver_name, .type = driver_type});

  const mir::ExprId attach = resolve_block.exprs.Add(
      mir::MakeNetAttachDriverCallExpr(net_access, driver_type));
  const mir::ExprId resolve_self =
      resolve_block.exprs.Add(MakeSelfRefExpr(resolve_frame, self_ptr_type));
  const mir::ExprId driver_lhs = resolve_block.exprs.Add(
      mir::MakeFieldAccessExpr(resolve_self, driver_field, driver_type));
  const mir::ExprId attach_assign = resolve_block.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = driver_lhs, .value = attach},
          .type = driver_type});
  resolve_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = attach_assign}});

  auto source_or = lowerer.LowerExpr(hir_scope.exprs.Get(source), init_frame);
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::ExprId seed_value = init_block.exprs.Add(*std::move(source_or));
  const auto init_self = [&] {
    return init_block.exprs.Add(MakeSelfRefExpr(init_frame, self_ptr_type));
  };
  const mir::ExprId seed_services = init_block.exprs.Add(
      mir::MakeServicesCallExpr(init_self(), unit.builtins.services));
  const mir::ExprId seed_driver = init_block.exprs.Add(
      mir::MakeFieldAccessExpr(init_self(), driver_field, driver_type));
  const mir::ExprId seed_update = init_block.exprs.Add(
      mir::MakeNetDriverUpdateCallExpr(
          seed_driver, seed_services, seed_value, unit.builtins.void_type));
  init_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = seed_update}});

  return AttachedDriver{
      .driver_field = driver_field, .driver_type = driver_type};
}

}  // namespace

// LRM 10.3.2 (continuous assignment) and LRM 9.2.2.2.1 (always_comb) share a
// runtime mental model: re-evaluate the assignment whenever any RHS read
// changes. HIR keeps continuous assignment as a distinct scope-level node so
// source diagnostics retain provenance; at HIR -> MIR we materialise the
// runtime shape as a coroutine body `forever { <write>; wait on reads; }`,
// which the caller registers as a startup activation. The body executes once
// at t=0 (the natural fall-through of the eternal loop) before the first
// wait, matching LRM 9.2.2.2's "evaluate at time 0" requirement for inferred
// sensitivity. The write is chosen by LHS type: a resolved-net cell is
// driven through a driver handle attached at Resolve; every other observable
// cell is written directly. Same-scope multi-driver on one net is rejected
// against `driven_nets`, which the caller carries across its loop.
auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    std::string name, const hir::ContinuousAssign& src,
    ContinuousAssignDrivenNets& driven_nets) -> diag::Result<mir::MethodDecl> {
  mir::CompilationUnit& unit = lowerer.Module().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const mir::TypeId self_ptr_type = ctor_frame.current_class->self_pointer_type;

  // A resolved-net target acquires a driver handle in Resolve, installed as
  // a field on the enclosing class; the body updates that handle every
  // iteration. A non-net LHS falls through to the direct observable write,
  // with no Resolve-phase acquisition. Multi-driver rejection runs before
  // the install so a duplicate never leaves side effects behind.
  std::optional<AttachedDriver> attached;
  if (const auto net =
          ClassifyLhsAsResolvedNet(lowerer, resolve_frame, src.lhs)) {
    if (std::ranges::find(driven_nets, net->target) != driven_nets.end()) {
      return diag::Fail(
          src.span, diag::DiagCode::kUnsupportedContinuousAssignForm,
          "a net with more than one driver is not yet supported");
    }
    driven_nets.push_back(net->target);
    auto net_access_or =
        lowerer.LowerLhsExpr(hir_scope.exprs.Get(src.lhs), resolve_frame);
    if (!net_access_or) {
      return std::unexpected(std::move(net_access_or.error()));
    }
    const mir::ExprId net_access =
        resolve_frame.current_block->exprs.Add(*std::move(net_access_or));
    const std::string driver_name = std::format("{}__driver", name);
    auto attached_or = AttachDriver(
        lowerer, net_access, net->value_type, driver_name, src.rhs,
        resolve_frame, init_frame);
    if (!attached_or) return std::unexpected(std::move(attached_or.error()));
    attached = *attached_or;
  }

  mir::CallableCode code;
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = self_ptr_type});

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithCoroutineBody(true).WithBlock(
          &body_block);

  auto rhs_or = lowerer.LowerExpr(hir_scope.exprs.Get(src.rhs), body_frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::TypeId assign_type = (*rhs_or).type;
  const mir::ExprId rhs_id = body_block.exprs.Add(*std::move(rhs_or));

  // Build `self.Services()` in the body: an observable LHS writes through
  // it, and a net driver updates through it. The body's `self` is the
  // receiver binding seeded above, reached through the same capture
  // machinery a process body's self read uses.
  const mir::ExprId body_self_ref =
      body_block.exprs.Add(MakeSelfRefExpr(body_frame, self_ptr_type));
  const mir::ExprId body_services_id = body_block.exprs.Add(
      mir::MakeServicesCallExpr(body_self_ref, unit.builtins.services));

  mir::ExprId effect_id{};
  if (attached.has_value()) {
    // Body writes the driver handle; the net re-resolves and publishes on a
    // real change (LRM 6.5).
    const mir::ExprId driver_self =
        body_block.exprs.Add(MakeSelfRefExpr(body_frame, self_ptr_type));
    const mir::ExprId driver_access = body_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            driver_self, attached->driver_field, attached->driver_type));
    effect_id = body_block.exprs.Add(
        mir::MakeNetDriverUpdateCallExpr(
            driver_access, body_services_id, rhs_id, unit.builtins.void_type));
  } else {
    auto lhs_or =
        lowerer.LowerLhsExpr(hir_scope.exprs.Get(src.lhs), body_frame);
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    const mir::ExprId lhs_id = body_block.exprs.Add(*std::move(lhs_or));
    const mir::Expr assign_expr = BuildObservableAssignExpr(
        unit, body_block, body_services_id, lhs_id, rhs_id, std::nullopt,
        assign_type, unit.builtins.void_type);
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
