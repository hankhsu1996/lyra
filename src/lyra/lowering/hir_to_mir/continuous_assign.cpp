#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/net_declaration.hpp"
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
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

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
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::Block& block,
    const AttachedDriver& driver) -> mir::ExprId {
  const mir::ExprId self = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  return block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(unit, block, self),
          mir::ClassFieldTarget{
              .owner = mir::IntraUnitClassRef{frame.current_class_id},
              .slot = driver.field},
          driver.type));
}

// Where a continuous driver of place `place` of what `destination` names is
// reached when a force on that variable ends, so that it evaluates again
// (LRM 10.6.2). Which storage a write lands in is known only where it lands, so
// the storage is asked, through a reference to it; storage nothing can force
// answers with nowhere.
auto ReestablishedPlace(
    mir::CompilationUnit& unit, const WalkFrame& frame,
    const DriveDestination& destination, std::size_t place)
    -> diag::Result<mir::ExprId> {
  auto driven = destination(frame);
  if (!driven) return std::unexpected(std::move(driven.error()));
  std::vector<PathOwner> owners;
  ForEachPlace(
      *driven, [&](const AccessPath& path) { owners.push_back(path.owner); });
  mir::Block& block = *frame.current_block;
  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = support::BuiltinFn::kReestablishedOf},
          {PathReference(
              unit, block, AccessPath{.owner = owners[place], .descent = {}})},
          mir::ErasedPointer(unit.types)));
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
// The target is an lvalue (LRM Table 10-1), and where each store lands follows
// the type of the place it reaches. A net accepts no store, only a drive (LRM
// 6.5): the assignment acquires a driver for it at Resolve and every store
// re-roots onto that driver's contribution, so several assignments to one net
// install independent drivers the net resolves. A place that names only part
// of a net drives only that part, because the rest of its driver's
// contribution stays at the resolution identity and keeps deferring to whoever
// drives it. Every other place is written where it is named.
auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    const hir::ContinuousAssign& src) -> diag::Result<mir::CallableDecl> {
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const hir::Expr& hir_lhs = hir_scope.exprs.Get(src.lhs);
  return LowerContinuousDrive(
      lowerer, ctor_frame, resolve_frame, init_frame,
      [&](const WalkFrame& frame) {
        return LowerLvalue(lowerer, hir_lhs, frame);
      },
      hir_scope.exprs.Get(src.rhs), src.strength, src.sensitivity_list);
}

auto LowerContinuousDrive(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    const DriveDestination& destination, const hir::Expr& source,
    support::StrengthLevel drive_strength,
    std::span<const hir::SensitivityEntry> sensitivity)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::TypeId self_ptr_type = ctor_frame.current_class->self_pointer_type;

  // What each place of the target is driven through: a net accepts no store,
  // only a drive, so each place that is one acquires a driver in Resolve, held
  // in a field of the enclosing class, and a variable has none. The place's
  // root decides it: the source may have named a whole net or a part of one,
  // and a part of a net is still a net.
  std::vector<std::optional<AttachedDriver>> drivers;
  {
    mir::Block& resolve_block = *resolve_frame.current_block;
    auto named = destination(resolve_frame);
    if (!named) return std::unexpected(std::move(named.error()));
    ForEachPlace(*named, [&](const AccessPath& place_path) {
      // A property of an object is a variable's storage, never a net's, so
      // only a place can be a net.
      const auto* place = std::get_if<mir::ExprId>(&place_path.owner);
      const auto* net =
          place == nullptr
              ? nullptr
              : unit.types.Get(resolve_block.exprs.Get(*place).type)
                    .As<mir::ResolvedType>();
      if (net == nullptr) {
        drivers.emplace_back(std::nullopt);
        return;
      }
      const mir::ExprId cell = *place;
      const mir::TypeId driver_type =
          unit.types.Intern(mir::Type{mir::DriverType{.value = net->value}});
      mir::Class& mir_class = *resolve_frame.current_class;
      const AttachedDriver driver{
          .field = mir_class.fields.Add(mir::FieldDecl{.type = driver_type}),
          .type = driver_type};
      const mir::ExprId strength =
          BuildStrengthOperand(unit, resolve_block, drive_strength);
      const mir::ExprId attach = resolve_block.exprs.Add(
          mir::MakeNetAttachDriverCallExpr(cell, strength, driver_type));
      const mir::ExprId handle =
          DriverAccess(unit, resolve_frame, resolve_block, driver);
      resolve_block.AppendStmt(
          mir::ExprStmt{
              .expr = resolve_block.exprs.Add(
                  mir::MakeAssignExpr(unit.builtins, handle, attach))});
      drivers.emplace_back(driver);
    });
  }

  // One evaluation: the right-hand side read once and each place written its
  // share, a net's re-rooted onto its driver's contribution. The seed below
  // asks for the driven places alone.
  const auto emit_stores = [&](const WalkFrame& frame,
                               bool only_driven) -> diag::Result<void> {
    mir::Block& block = *frame.current_block;
    auto value_or = lowerer.LowerExpr(source, frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value = block.exprs.Add(*std::move(value_or));
    auto named = destination(frame);
    if (!named) return std::unexpected(std::move(named.error()));
    auto shares = Shares(lowerer.Owner(), frame, *named, value);
    if (!shares) return std::unexpected(std::move(shares.error()));
    for (std::size_t i = 0; i < shares->size(); ++i) {
      Share& share = (*shares)[i];
      const std::optional<AttachedDriver>& driver = drivers[i];
      if (driver.has_value()) {
        share.place.owner = DriverAccess(unit, frame, block, *driver);
      } else if (only_driven) {
        continue;
      }
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.exprs.Add(
                  BuildStoreExpr(unit, block, share.place, share.value))});
    }
    return {};
  };

  // A driver that has attached but not yet driven contributes the resolution
  // identity, so a net would read as undriven to anything that reads it before
  // the body first runs -- including another unit's Initialize, which the
  // parent-first order can place after this one. Seeding the contribution in
  // Initialize is what closes that window. A variable needs no seed: it holds
  // its declared initial value until the body's own first pass.
  if (std::ranges::any_of(
          drivers, [](const auto& d) { return d.has_value(); })) {
    if (auto seeded = emit_stores(init_frame, true); !seeded) {
      return std::unexpected(std::move(seeded.error()));
    }
  }

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id =
      bindings.Declare(BindingOriginId::Receiver(), self_ptr_type);

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block);

  if (auto stored = emit_stores(body_frame, false); !stored) {
    return std::unexpected(std::move(stored.error()));
  }

  // A net keeps each driver's contribution while it is forced and resolves
  // them again at the release, so only a variable's driver has to be told.
  std::vector<StatedPlace> reestablished;
  for (std::size_t i = 0; i < drivers.size(); ++i) {
    if (drivers[i].has_value()) continue;
    reestablished.emplace_back([&, i](const WalkFrame& in) {
      return ReestablishedPlace(unit, in, destination, i);
    });
  }
  auto waited = BuildValueChangeWaitStmt(
      body_block, body_frame, lowerer, sensitivity, reestablished);
  if (!waited) return std::unexpected(std::move(waited.error()));
  body_block.AppendStmt(*std::move(waited));

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
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt};
}

}  // namespace lyra::lowering::hir_to_mir
