#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"

#include <cstddef>
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

  // The escaping scope's locals are promoted into a compiler-generated struct
  // whose identity lives in the unit's struct registry. The name is unique
  // within the unit, which is the scope a struct identity has to be
  // distinguishable in.
  const std::string struct_name = std::string(process.CallableName()) +
                                  "__scope" +
                                  std::to_string(unit.structs.size());
  mir::StructDecl struct_decl;
  struct_decl.name = struct_name;
  std::vector<mir::FieldId> fields;
  fields.reserve(promoted.size());
  for (const hir::ProceduralVarId v : promoted) {
    const hir::ProceduralVarDecl& decl = body.procedural_vars.Get(v);
    fields.push_back(struct_decl.fields.Add(
        mir::FieldDecl{
            .name = decl.name, .type = unit_lowerer.TranslateType(decl.type)}));
  }
  const mir::StructId struct_id = unit.AddStruct(std::move(struct_decl));
  const mir::TypeId struct_type =
      unit.types.Intern(mir::StructType{.struct_id = struct_id});

  // The handle: a shared pointer to the generated struct, allocated by
  // make_shared. Declared first in the scope, before the promoted locals it
  // stands in for.
  const mir::TypeId handle_type =
      unit.types.PointerTo(struct_type, mir::PointerOwnership::kShared);
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
  const mir::LocalId handle = frame.bindings->Declare(
      handle_origin,
      mir::LocalDecl{.name = struct_name + "_h", .type = handle_type});
  block.AppendStmt(mir::LocalDeclStmt{.target = handle, .init = init});

  for (std::size_t i = 0; i < promoted.size(); ++i) {
    process.RecordPendingActivation(
        promoted[i], PromotedVarBinding{
                         .handle_origin = handle_origin,
                         .handle_type = handle_type,
                         .field = fields[i]});
  }
}

auto CancellationTargetType(mir::CompilationUnit& unit) -> mir::TypeId {
  return unit.types.Intern(
      mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationTarget});
}

// The target of a block a `disable` can name (LRM 9.6.2), absent when it is
// none. A `disable` reaches its target by naming it, so an unnamed block is one
// no `disable` can reach and it needs no region -- which is what owning no
// target says.
auto BlockCancellationTarget(
    const ProcessLowerer& process, const hir::BlockStmt& b)
    -> std::optional<mir::FieldId> {
  // A target is per-instance storage read off the body's own object, so a body
  // that reaches none can hold none.
  if (!process.BodyHasReceiver()) {
    return std::nullopt;
  }
  return process.Scopes().Get(b.scope).cancellation_target;
}

// The expression reaching the target a region claims and a `disable`
// invalidates (LRM 9.6.2). A target is storage the enclosing instance owns
// rather than a value, so what every operation on one takes is its address.
auto CancellationTarget(
    ProcessLowerer& process, const WalkFrame& frame, mir::FieldId target)
    -> mir::ExprId {
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::ExprId member = block.exprs.Add(BuildStructuralFieldAccessExpr(
      frame, unit, mir::EnclosingHops{}, target));
  return block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = member},
          .type = unit.types.PointerTo(
              CancellationTargetType(unit), mir::PointerOwnership::kBorrowed)});
}

// Appends one end of a target's extent -- entering it or leaving it -- as a
// statement of `frame`'s block.
void EmitTargetBracket(
    ProcessLowerer& process, const WalkFrame& frame, mir::FieldId target,
    support::BuiltinFn bracket) {
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
    mir::FieldId target) -> mir::TryStmt {
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
      mir::RuntimeLibraryType{.kind = mir::RuntimeLibraryKind::kControlEffect});
  const BindingOriginId origin =
      BindingOriginId::Synthesized(unit_lowerer.NextSynthesizedSite(), 0);
  const mir::LocalId caught = frame.bindings->Declare(
      origin, mir::LocalDecl{
                  .name = "effect_" + std::to_string(target.value),
                  .type = effect_type});

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
  // -- leaves the block and resumes just past it. An unnamed block is an
  // ordinary one and needs nothing.
  const std::optional<mir::FieldId> cancel_target =
      BlockCancellationTarget(process, b);

  const hir::ProceduralBody& hir_proc = process.HirBody();
  for (const hir::StmtId child_hir_id : b.statements) {
    auto child_or =
        process.LowerStmt(hir_proc.stmts.Get(child_hir_id), child_frame);
    if (!child_or) return std::unexpected(std::move(child_or.error()));
    child_block.AppendStmt(*std::move(child_or));
  }
  // The handler is built where the region sits, not inside its body: it runs
  // once the body has already been left.
  if (cancel_target.has_value()) {
    return mir::Stmt{
        .label = std::move(label),
        .data = BuildCancellableRegion(
            process, frame, std::move(child_block), *cancel_target)};
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

  // Reaching a target means projecting its per-instance storage from the
  // disabling body's own object; a body that reaches none -- a package callable
  // (LRM 26.3), a static class method (LRM 8.10) -- cannot name a target.
  if (!process.BodyHasReceiver()) {
    return diag::Fail(
        diag::SourceSpan{}, diag::DiagCode::kUnsupportedStatementForm,
        "disable from a body that has no enclosing instance is not yet "
        "supported");
  }
  const std::optional<mir::FieldId> target =
      process.Scopes().Get(d.target).cancellation_target;
  if (!target.has_value()) {
    throw InternalError(
        "LowerDisableStmt: the named scope owns no cancellation target, so no "
        "name could have reached it -- please report this as a bug");
  }
  const mir::ExprId member = CancellationTarget(process, frame, *target);
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
