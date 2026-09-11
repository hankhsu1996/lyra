#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 6.21: a block declaring automatic locals a detached fork branch borrows
// and can outlive lifts the whole borrowed set into one shared promoted scope
// (the "activation frame" role). Synthesize a struct holding those locals as
// fields, allocate it at block entry through a shared handle, and record each
// promoted var's field so its declaration and references reach `handle->field`.
// The branch keeps the scope alive by holding a by-value copy of the handle.
void OpenActivationScope(
    ProcessLowerer& process, const WalkFrame& frame,
    std::span<const hir::StmtId> statements) {
  const hir::ProceduralBody& body = process.HirBody();
  std::vector<hir::ProceduralVarId> promoted;
  for (const hir::StmtId sid : statements) {
    const auto* vd = std::get_if<hir::VarDeclStmt>(&body.stmts.Get(sid).data);
    if (vd != nullptr && body.procedural_vars.Get(vd->var).lifetime_extended) {
      promoted.push_back(vd->var);
    }
  }
  if (promoted.empty()) {
    return;
  }

  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();

  // The escaping scope's locals are gathered into a compiler-generated struct
  // whose identity lives in the unit's struct registry. That identity is the
  // whole of what distinguishes it; the source declared no such aggregate and
  // so wrote no name for one.
  mir::StructDecl struct_decl;
  std::vector<mir::FieldId> fields;
  fields.reserve(promoted.size());
  for (const hir::ProceduralVarId v : promoted) {
    const hir::ProceduralVarDecl& decl = body.procedural_vars.Get(v);
    fields.push_back(struct_decl.fields.Add(
        mir::FieldDecl{.type = unit_lowerer.TranslateType(decl.type)}));
  }
  const mir::StructId struct_id = unit.AddStruct(std::move(struct_decl));
  const mir::TypeId struct_type =
      unit.types.Intern(mir::Type{mir::StructType{.struct_id = struct_id}});

  // The handle: a shared pointer to the generated struct, allocated by
  // make_shared. Declared first in the scope, before the promoted locals it
  // stands in for.
  const mir::TypeId handle_type = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = struct_type,
          .ownership = mir::PointerOwnership::kShared,
          .mutability = mir::Mutability::kMutable}});
  mir::Block& block = *frame.current_block;
  const mir::ExprId init = block.exprs.Add(
      mir::Expr{
          .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
          .type = handle_type});
  // The handle is a synthesized carrier declared in this body and captured (by
  // value, owning) by any branch that borrows a promoted field. Its origin
  // comes from the unit's synthesized-site allocator, the one collision-free id
  // space every synthesized carrier shares.
  const BindingOriginId handle_origin =
      BindingOriginId::Synthesized(unit_lowerer.NextSynthesizedSite(), 0);
  const mir::LocalId handle =
      frame.bindings->Declare(handle_origin, handle_type);
  block.AppendStmt(mir::LocalDeclStmt{.target = handle, .init = init});

  for (std::size_t i = 0; i < promoted.size(); ++i) {
    process.RecordPendingActivation(
        promoted[i], PromotedVarBinding{
                         .handle_origin = handle_origin,
                         .handle_type = handle_type,
                         .field = mir::StructFieldTarget{
                             .owner = struct_id, .slot = fields[i]}});
  }
}

auto CancellationTargetType(mir::CompilationUnit& unit) -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationTarget}});
}

// The expression reaching the target a region claims and a `disable`
// invalidates (LRM 9.6.2). A target is storage rather than a value, so what
// every operation on one takes is its address.
auto CancellationTarget(
    ProcessLowerer& process, const WalkFrame& frame,
    const StaticStorageHome& target) -> mir::ExprId {
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId target_type = CancellationTargetType(unit);
  const mir::ExprId member = block.exprs.Add(BuildStaticStorageAccess(
      unit, frame, target, target_type, mir::EnclosingHops{}));
  return block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = member},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = target_type,
                  .ownership = mir::PointerOwnership::kBorrowed}})});
}

// Appends one end of a target's extent -- entering it or leaving it -- as a
// statement of `frame`'s block.
void EmitTargetBracket(
    ProcessLowerer& process, const WalkFrame& frame,
    const StaticStorageHome& target, support::BuiltinFn bracket) {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  const mir::ExprId reached = CancellationTarget(process, frame, target);
  const mir::ExprId services =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = bracket},
                  .arguments = {services, reached}},
          .type = unit.builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = call});
}

}  // namespace

auto BuildCancellableRegion(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block&& body,
    const StaticStorageHome& target) -> mir::TryStmt {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  // Entering the target is the region's own first act, so an execution inside
  // it -- including one suspended in a callable the body invoked -- is known to
  // be inside the target; the cleanup withdraws that however control gets out.
  mir::Block region_body;
  const WalkFrame region_frame = frame.WithBlock(&region_body);
  EmitTargetBracket(
      process, region_frame, target, support::BuiltinFn::kEnterTarget);

  mir::Block cleanup;
  const WalkFrame cleanup_frame = frame.WithBlock(&cleanup);
  EmitTargetBracket(
      process, cleanup_frame, target, support::BuiltinFn::kLeaveTarget);

  const mir::BlockId body_id = region_body.child_scopes.Add(std::move(body));
  const mir::BlockId cleanup_id =
      region_body.child_scopes.Add(std::move(cleanup));
  region_body.AppendStmt(
      mir::FinallyStmt{.body = body_id, .cleanup = cleanup_id});

  const mir::TypeId effect_type = unit.types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kControlEffect}});
  const std::uint32_t site = unit_lowerer.NextSynthesizedSite();
  const BindingOriginId origin = BindingOriginId::Synthesized(site, 0);
  const mir::LocalId caught = frame.bindings->Declare(origin, effect_type);

  // The handler is a scope of its own, so its test and its raise are lowered
  // through a frame whose current block is that scope.
  mir::Block handler;
  const WalkFrame handler_frame = frame.WithBlock(&handler);
  const mir::ExprId caught_ref =
      handler.exprs.Add(mir::MakeLocalRefExpr(caught, effect_type));
  const mir::ExprId reached =
      CancellationTarget(process, handler_frame, target);
  const mir::ExprId claims = handler.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kEffectNamesTarget},
                  .arguments = {caught_ref, reached}},
          .type = unit.builtins.bit1});
  const mir::ExprId declined = handler.exprs.Add(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot, .operand = claims},
          .type = unit.builtins.bit1});

  mir::Block decline;
  const mir::ExprId raised =
      decline.exprs.Add(mir::MakeLocalRefExpr(caught, effect_type));
  decline.AppendStmt(mir::RaiseStmt{.effect = raised});
  handler.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(unit, handler, declined),
          .then_scope = handler.child_scopes.Add(std::move(decline)),
          .else_scope = std::nullopt});

  const mir::BlockId region_body_id =
      block.child_scopes.Add(std::move(region_body));
  return mir::TryStmt{
      .body = region_body_id,
      .caught = caught,
      .handler = block.child_scopes.Add(std::move(handler))};
}

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
}

auto LowerBlockStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::BlockStmt& b) -> diag::Result<mir::Stmt> {
  mir::Block child_block;
  // A block is part of the hierarchical name of everything inside it, so
  // entering it adopts the name node the shape phase gave it.
  const WalkFrame child_frame =
      frame.WithBlock(&child_block)
          .WithScopeNameBorrowedHandle(
              process.Scopes().Get(b.scope).NameBorrowedHandle());
  OpenActivationScope(process, child_frame, b.statements);

  // A named block (LRM 9.6.2) is a region that consumes the effect naming it:
  // an execution anywhere inside it -- including inside a callable it invoked
  // -- leaves the block and resumes just past it. A `disable` reaches its
  // target by naming it, so an unnamed block is one none can reach and it needs
  // no region -- which is what owning no target says.
  const std::optional<StaticStorageHome>& disable_target =
      process.Scopes().Get(b.scope).disable_target;

  const hir::ProceduralBody& hir_proc = process.HirBody();
  for (const hir::StmtId child_hir_id : b.statements) {
    auto child_or =
        process.LowerStmt(hir_proc.stmts.Get(child_hir_id), child_frame);
    if (!child_or) return std::unexpected(std::move(child_or.error()));
    child_block.AppendStmt(*std::move(child_or));
  }
  // The handler is built where the region sits, not inside its body: it runs
  // once the body has already been left.
  if (disable_target.has_value()) {
    return mir::Stmt{
        .label = std::move(label),
        .data = BuildCancellableRegion(
            process, frame, std::move(child_block), *disable_target)};
  }
  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(child_block));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

auto LowerDisableStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::DisableStmt& d) -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  // Both forms hand the statement the one thing it acts on: the target's
  // address. A scope of this body's own declaration scope is a cell reached
  // where it sits; one anywhere else was sealed by a route in the resolve
  // phase, so the statement reads the slot and walks nothing.
  const mir::ExprId member = std::visit(
      Overloaded{
          [&](const hir::DirectDisableTarget& t) {
            const std::optional<StaticStorageHome>& home =
                process.Scopes().Get(t.scope).disable_target;
            if (!home.has_value()) {
              throw InternalError(
                  "LowerDisableStmt: the named scope owns no disable target, "
                  "so the source named it nothing and no name could have "
                  "reached it -- please report this as a bug");
            }
            return CancellationTarget(process, frame, *home);
          },
          [&](const hir::RoutedDisableTarget& t) {
            return block.exprs.Add(BuildStructuralFieldAccessExpr(
                frame, unit, mir::EnclosingHops{0},
                process.RoutedRefTarget(t.target.id).target));
          }},
      d.target);
  const mir::ExprId services =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  // One call carries the whole statement (LRM 9.6.2): it invalidates the
  // target, wakes what is blocked inside it, and -- when the disabling
  // execution is itself inside the target -- leaves from here, which is what a
  // self-disable means. Nothing about where any affected execution lands is
  // decided here; each leaves through the region that names the target.
  const mir::ExprId disable = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = support::BuiltinFn::kDisable},
                  .arguments = {member, services}},
          .type = unit.builtins.void_type});
  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = disable}};
}

auto LowerStmtIntoChildScope(
    ProcessLowerer& process, WalkFrame frame, hir::StmtId hir_stmt_id)
    -> diag::Result<mir::Block> {
  mir::Block child_block;
  const WalkFrame child_frame = frame.WithBlock(&child_block);
  const hir::Stmt& hir_stmt = process.HirBody().stmts.Get(hir_stmt_id);
  auto lowered = process.LowerStmt(hir_stmt, child_frame);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  child_block.AppendStmt(*std::move(lowered));
  return child_block;
}

}  // namespace lyra::lowering::hir_to_mir
