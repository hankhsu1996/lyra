#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"

#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>

#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The driver a continuous assign drives its target net through. A field on the
// enclosing class holds the handle; it is attached at Resolve, and every store
// the assignment makes -- the Initialize seed and each body re-evaluation --
// reaches the net through it.
struct AttachedDriver {
  mir::FieldId field;
  mir::TypeId type;
};

// The class field access reaching an attached driver from `frame`'s body.
auto DriverAccess(
    const WalkFrame& frame, mir::Block& block, const AttachedDriver& driver)
    -> mir::ExprId {
  const mir::ExprId self = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  return block.exprs.Add(
      mir::MakeFieldAccessExpr(
          self,
          mir::FieldTarget{
              .owner = frame.current_class_id, .slot = driver.field},
          driver.type));
}

}  // namespace

// LRM 10.3.2 (continuous assignment) and LRM 9.2.2.2.1 (always_comb) share a
// runtime mental model: re-evaluate the assignment whenever any RHS read
// changes. HIR keeps continuous assignment as a distinct scope-level node so
// source diagnostics retain provenance; at HIR -> MIR we materialise the
// runtime shape as a coroutine body `forever { <store>; wait on reads; }`,
// which the caller registers as a startup activation. The body executes once
// at t=0 (the natural fall-through of the eternal loop) before the first
// wait, matching LRM 9.2.2.2's "evaluate at time 0" requirement for inferred
// sensitivity.
//
// Where the store lands follows the target's own type. A net accepts no store,
// only a drive (LRM 6.5): its assignment acquires a driver at Resolve and every
// store re-roots onto that driver's contribution, so several assignments to one
// net install independent drivers the net resolves. An assignment that names
// only part of a net drives only that part, because the rest of its driver's
// contribution stays at the resolution identity and keeps deferring to whoever
// drives it. Every other target is written where it is named.
auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    std::string name, const hir::ContinuousAssign& src)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const mir::TypeId self_ptr_type = ctor_frame.current_class->self_pointer_type;
  const hir::Expr& hir_lhs = hir_scope.exprs.Get(src.lhs);
  const hir::Expr& hir_rhs = hir_scope.exprs.Get(src.rhs);

  // The store's destination in one frame: the target as the source named it,
  // re-rooted onto the driver when the target is a net. `driver` is absent
  // until the Resolve pass below decides the target is one.
  std::optional<AttachedDriver> driver;
  const auto lower_destination =
      [&](const WalkFrame& frame) -> diag::Result<mir::ExprId> {
    mir::Block& block = *frame.current_block;
    auto named_or = lowerer.LowerLhsExpr(hir_lhs, frame);
    if (!named_or) return std::unexpected(std::move(named_or.error()));
    const mir::ExprId named = block.exprs.Add(*std::move(named_or));
    if (!driver.has_value()) return named;
    return ReplaceLhsRoot(
        unit, block, named, DriverAccess(frame, block, *driver));
  };

  // A net target acquires its driver in Resolve, installed as a field on the
  // enclosing class. The target's root decides this: the source may have named
  // the whole net or a part of it, and a part of a net is still a net.
  {
    mir::Block& resolve_block = *resolve_frame.current_block;
    auto named_or = lowerer.LowerLhsExpr(hir_lhs, resolve_frame);
    if (!named_or) return std::unexpected(std::move(named_or.error()));
    const mir::ExprId named = resolve_block.exprs.Add(*std::move(named_or));
    const mir::ExprId cell = FindLhsRootId(unit, resolve_block, named);
    if (const auto* net = unit.types.Get(resolve_block.exprs.Get(cell).type)
                              .As<mir::ResolvedType>()) {
      const mir::TypeId driver_type = unit.types.Intern(
          mir::Type{mir::DriverType{
              .value = net->value, .resolution = net->resolution}});
      mir::Class& mir_class = *resolve_frame.current_class;
      driver = AttachedDriver{
          .field = mir_class.fields.Add(
              mir::FieldDecl{
                  .name = std::format("{}__driver", name),
                  .type = driver_type}),
          .type = driver_type};
      const mir::ExprId attach = resolve_block.exprs.Add(
          mir::MakeNetAttachDriverCallExpr(cell, driver_type));
      const mir::ExprId handle =
          DriverAccess(resolve_frame, resolve_block, *driver);
      resolve_block.AppendStmt(
          mir::ExprStmt{
              .expr = resolve_block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::AssignExpr{.target = handle, .value = attach},
                      .type = driver_type})});
    }
  }

  const auto emit_store = [&](const WalkFrame& frame) -> diag::Result<void> {
    mir::Block& block = *frame.current_block;
    auto value_or = lowerer.LowerExpr(hir_rhs, frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value = block.exprs.Add(*std::move(value_or));
    const mir::TypeId value_type = block.exprs.Get(value).type;
    auto destination_or = lower_destination(frame);
    if (!destination_or) {
      return std::unexpected(std::move(destination_or.error()));
    }
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(BuildStoreExpr(
                unit, block, *destination_or, value, std::nullopt,
                value_type))});
    return {};
  };

  // A driver that has attached but not yet driven contributes the resolution
  // identity, so a net would read as undriven to anything that reads it before
  // the body first runs -- including another unit's Initialize, which the
  // parent-first order can place after this one. Seeding the contribution in
  // Initialize is what closes that window. A variable target needs no seed: it
  // holds its declared initial value until the body's own first pass.
  if (driver.has_value()) {
    if (auto seeded = emit_store(init_frame); !seeded) {
      return std::unexpected(std::move(seeded.error()));
    }
  }

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = self_ptr_type});

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block);

  if (auto stored = emit_store(body_frame); !stored) {
    return std::unexpected(std::move(stored.error()));
  }

  body_block.AppendStmt(BuildValueChangeWaitStmt(
      body_block, body_frame, lowerer, src.sensitivity_list));

  const mir::BlockId body_scope_id =
      code.Body().child_scopes.Add(std::move(body_block));
  code.Body().AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;
  return mir::CallableDecl{
      .name = std::move(name),
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt};
}

}  // namespace lyra::lowering::hir_to_mir
