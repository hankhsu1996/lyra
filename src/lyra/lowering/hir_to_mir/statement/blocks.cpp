#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
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
    ProcessLowerer& process, const WalkFrame& frame, const hir::BlockStmt& b) {
  const hir::ProceduralBody& body = process.HirBody();
  std::vector<hir::ProceduralVarId> promoted;
  for (const hir::StmtId sid : b.statements) {
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

auto CancellationSourceType(mir::CompilationUnit& unit) -> mir::TypeId {
  return unit.types.Intern(
      mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationSource});
}

auto CancellationGuardType(mir::CompilationUnit& unit) -> mir::TypeId {
  return unit.types.Intern(
      mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationGuard});
}

// The storage holding a block's cancellation source (LRM 9.6.2), or nullopt
// when the block carries no SV name. A `disable` reaches its target by naming
// it, so an unnamed block is one no `disable` can reach and it needs no region.
auto BlockCancellationSource(
    const ProcessLowerer& process, const hir::BlockStmt& b)
    -> std::optional<StaticStoragePlacement> {
  // The source is per-instance storage projected from the body's own object, so
  // a body that reaches none can hold no target.
  if (!b.scope.has_value() || !process.BodyHasReceiver()) {
    return std::nullopt;
  }
  const MaterializedProceduralScope& scope =
      process.StoragePlan().ScopeMaterialization(*b.scope);
  if (!scope.materialized) {
    return std::nullopt;
  }
  return scope.cancellation_source;
}

}  // namespace

// The lvalue reaching a scope's cancellation source, projected from `self`
// through whatever storage owner the plan placed it on.
auto CancellationSourceAccess(
    const ProcessLowerer& process, const WalkFrame& frame,
    StaticStoragePlacement placement) -> mir::ExprId {
  return frame.current_block->exprs.Add(
      process.BuildStaticStorageAccess(frame, placement));
}

auto EmitCancellationGuard(
    ProcessLowerer& process, const WalkFrame& frame,
    StaticStoragePlacement placement) -> mir::LocalId {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  const mir::TypeId source_ptr = unit.types.PointerTo(
      CancellationSourceType(unit), mir::PointerOwnership::kBorrowed);
  const mir::TypeId guard_type = CancellationGuardType(unit);

  const mir::ExprId member =
      CancellationSourceAccess(process, frame, placement);
  const mir::ExprId addr = block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = member}, .type = source_ptr});
  const mir::ExprId services =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId construct = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{}, .arguments = {services, addr}},
          .type = guard_type});

  const BindingOriginId origin =
      BindingOriginId::Synthesized(unit_lowerer.NextSynthesizedSite(), 0);
  const mir::LocalId guard = frame.bindings->Declare(
      origin,
      mir::LocalDecl{
          .name = "cancel_guard_" + std::to_string(placement.field.value),
          .type = guard_type});
  block.AppendStmt(mir::LocalDeclStmt{.target = guard, .init = construct});
  return guard;
}

auto MakeCancellableRegion(
    ProcessLowerer& process, const WalkFrame& frame, mir::BlockId body,
    StaticStoragePlacement placement) -> mir::TryStmt {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  const mir::TypeId abort_type = unit.types.Intern(
      mir::RuntimeLibraryType{.kind = mir::RuntimeLibraryKind::kAbort});
  const BindingOriginId origin =
      BindingOriginId::Synthesized(unit_lowerer.NextSynthesizedSite(), 0);
  const mir::LocalId caught = frame.bindings->Declare(
      origin, mir::LocalDecl{
                  .name = "abort_" + std::to_string(placement.field.value),
                  .type = abort_type});

  const mir::ExprId caught_ref =
      block.exprs.Add(mir::MakeLocalRefExpr(caught, abort_type));
  const mir::ExprId target =
      CancellationSourceAccess(process, frame, placement);
  const mir::ExprId handler = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kAbortConsumeOrRethrow},
                  .arguments = {caught_ref, target}},
          .type = unit.builtins.void_type});
  return mir::TryStmt{.body = body, .caught = caught, .handler = handler};
}

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
}

auto LowerBlockStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::BlockStmt& b) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block child_block;
  const WalkFrame child_frame = frame.WithBlock(&child_block);
  OpenActivationScope(process, child_frame, b);

  // A named block (LRM 9.6.2) is a region that consumes the effect naming it:
  // an execution anywhere inside it -- including inside a callable it invoked
  // -- leaves the block and resumes just past it. An unnamed block is an
  // ordinary one and needs nothing.
  const std::optional<StaticStoragePlacement> cancel_source =
      BlockCancellationSource(process, b);
  if (cancel_source.has_value()) {
    // Entering the target is the body's own first act, so an execution inside
    // the body -- including one suspended in a callable it invoked -- is known
    // to be inside the target, and leaving the body leaves it.
    EmitCancellationGuard(process, child_frame, *cancel_source);
  }

  for (const hir::StmtId child_hir_id : b.statements) {
    const hir::Stmt& child = hir_proc.stmts.Get(child_hir_id);
    auto lowered = process.LowerStmt(child, child_frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    child_block.AppendStmt(*std::move(lowered));
  }
  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(child_block));
  // The handler is built where the region sits, not inside its body: it runs
  // once the body has already been left.
  if (cancel_source.has_value()) {
    return mir::Stmt{
        .label = std::move(label),
        .data =
            MakeCancellableRegion(process, frame, scope_id, *cancel_source)};
  }
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

auto LowerDisableStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::DisableStmt& d) -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  // Reaching the target means projecting its per-instance source from the
  // disabling body's own object; a body that reaches none -- a package callable
  // (LRM 26.3), a static class method (LRM 8.10) -- cannot name a target.
  const std::optional<StaticStoragePlacement> placement =
      process.BodyHasReceiver()
          ? process.StoragePlan()
                .ScopeMaterialization(d.target)
                .cancellation_source
          : std::nullopt;
  if (!placement.has_value()) {
    return diag::Fail(
        diag::SourceSpan{}, diag::DiagCode::kUnsupportedStatementForm,
        "disable from a body that has no enclosing instance is not yet "
        "supported");
  }
  const mir::ExprId member =
      CancellationSourceAccess(process, frame, *placement);
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
  return std::move(child_block);
}

}  // namespace lyra::lowering::hir_to_mir
