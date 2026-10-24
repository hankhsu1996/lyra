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
#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/target_landing.hpp"

namespace lyra::lowering::hir_to_mir {

void OpenActivationScope(
    ProcessLowerer& process, const WalkFrame& frame,
    std::span<const hir::ProceduralVarId> declared) {
  const hir::ProceduralBody& body = process.HirBody();
  std::vector<hir::ProceduralVarId> promoted;
  for (const hir::ProceduralVarId var : declared) {
    if (body.procedural_vars.Get(var).lifetime_extended) {
      promoted.push_back(var);
    }
  }
  if (promoted.empty()) {
    return;
  }

  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();

  // Each handle is a synthesized carrier declared in this body, before the
  // locals they stand in for, and captured (by value, owning) by any branch
  // that borrows its local. Their origins come from the unit's
  // synthesized-site allocator, the one collision-free id space every
  // synthesized carrier shares. A cell is built empty, and the local's
  // declaration initializes it where it is reached.
  const std::uint32_t site = unit_lowerer.NextSynthesizedSite();
  mir::Block& block = *frame.current_block;
  for (std::size_t i = 0; i < promoted.size(); ++i) {
    const mir::TypeId cell_type = mir::ObservableCellOf(
        unit.types,
        unit_lowerer.TranslateType(body.procedural_vars.Get(promoted[i]).type));
    const mir::TypeId handle_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = cell_type,
            .ownership = mir::PointerOwnership::kShared,
            .mutability = mir::Mutability::kMutable}});
    const BindingOriginId handle_origin = SynthesizedOrigin{
        .owner_site = site, .ordinal = static_cast<std::uint32_t>(i)};
    const mir::LocalId handle =
        frame.bindings->Declare(handle_origin, handle_type);
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = handle,
            .init = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee = mir::Construct{}, .arguments = {}},
                    .type = handle_type})});
    process.RecordPendingActivation(
        promoted[i],
        PromotedVarBinding{
            .handle_origin = handle_origin, .cell_type = cell_type});
  }
}

namespace {

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
  AppendRuntimeEffectStmt(
      process.Owner(), *frame.current_block, bracket,
      {CancellationTarget(process, frame, target)});
}

// The target's extent around `body`: `enter` first, so an execution inside it
// -- including one suspended in a callable the body invoked -- is known to be
// inside the target, and a cleanup withdrawing that however control gets out.
auto BuildExtent(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block&& body,
    const StaticStorageHome& target, support::BuiltinFn enter) -> mir::Block {
  mir::Block extent;
  EmitTargetBracket(process, frame.WithBlock(&extent), target, enter);
  mir::Block cleanup;
  EmitTargetBracket(
      process, frame.WithBlock(&cleanup), target,
      support::BuiltinFn::kLeaveTarget);
  extent.AppendFinally(std::move(body), std::move(cleanup));
  return extent;
}

// The region around `extent`, which lands an effect where the `disable` it
// carries lands here and raises it again otherwise.
auto BuildRegion(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block&& extent,
    const StaticStorageHome& target) -> mir::TryStmt {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  const mir::TypeId effect_type = unit.types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kControlEffect}});
  const std::uint32_t site = unit_lowerer.NextSynthesizedSite();
  const BindingOriginId origin =
      SynthesizedOrigin{.owner_site = site, .ordinal = 0};
  const mir::LocalId caught = frame.bindings->Declare(origin, effect_type);

  // The handler is a scope of its own, so its test and its raise are lowered
  // through a frame whose current block is that scope.
  mir::Block handler;
  const WalkFrame handler_frame = frame.WithBlock(&handler);
  const mir::ExprId caught_ref = handler.ReadLocal(caught, effect_type);
  const mir::ExprId reached =
      CancellationTarget(process, handler_frame, target);
  const mir::ExprId services =
      handler.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId lands = handler.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kRegionLandsEffect},
                  .arguments = {services, caught_ref, reached}},
          .type = unit.builtins.bit1});
  const mir::ExprId declined = BuildLogicalNot(unit, handler, lands);

  mir::Block decline;
  const mir::ExprId raised = decline.ReadLocal(caught, effect_type);
  decline.AppendStmt(mir::RaiseStmt{.effect = raised});
  handler.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(unit, handler, declined),
          .then_scope = handler.child_scopes.Add(std::move(decline)),
          .else_scope = std::nullopt});

  const mir::BlockId extent_id = block.child_scopes.Add(std::move(extent));
  return mir::TryStmt{
      .body = extent_id,
      .caught = caught,
      .handler = block.child_scopes.Add(std::move(handler))};
}

}  // namespace

auto BuildTargetExtent(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block&& body,
    const StaticStorageHome& target, support::TargetLanding landing)
    -> mir::StmtData {
  switch (landing) {
    case support::TargetLanding::kPastTheTarget:
      return BuildRegion(
          process, frame,
          BuildExtent(
              process, frame, std::move(body), target,
              support::BuiltinFn::kEnterTargetLandingPastIt),
          target);
    case support::TargetLanding::kAtExecutionEnd:
      return mir::BlockStmt{
          .scope = frame.current_block->child_scopes.Add(BuildExtent(
              process, frame, std::move(body), target,
              support::BuiltinFn::kEnterTargetLandingAtExecutionEnd))};
  }
  throw InternalError("BuildTargetExtent: unknown TargetLanding");
}

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
}

auto LowerBlockStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::BlockStmt& b, support::TargetLanding landing)
    -> diag::Result<mir::Stmt> {
  mir::Block child_block;
  // A block the source named is part of the name everything inside it reports
  // (LRM 21.2.1.5).
  const WalkFrame child_frame =
      frame.WithBlock(&child_block)
          .WithScopeName(process.Scopes().Get(b.scope).name);
  const hir::ProceduralBody& hir_proc = process.HirBody();
  std::vector<hir::ProceduralVarId> declared;
  for (const hir::StmtId statement : b.statements) {
    if (const auto* declaration = std::get_if<hir::VarDeclStmt>(
            &hir_proc.stmts.Get(statement).data)) {
      declared.push_back(declaration->var);
    }
  }
  OpenActivationScope(process, child_frame, declared);
  // A covergroup sampled where the block begins or ends samples there (LRM
  // 19.3). Its end is owed on every normal way out -- falling off the end, or
  // a jump out of the block -- and on none a departure takes.
  const ScopeBlockEvents& events = process.Scopes().Get(b.scope).block_events;
  AppendBlockEventTriggers(process, child_frame, events.at_begin);
  const NormalCleanup end{
      .emit =
          [&](const WalkFrame& at) {
            AppendBlockEventTriggers(process, at, events.at_end);
          },
      .outer = frame.normal_cleanups};
  const WalkFrame inside =
      events.at_end.empty() ? child_frame : child_frame.WithNormalCleanup(end);

  // A named block (LRM 9.6.2) is a target: an execution anywhere inside it --
  // including inside a callable it invoked -- leaves the block where it is
  // disabled, and lands where `landing` says. A `disable` reaches its target by
  // naming it, so an unnamed block is one none can reach and it needs no
  // extent -- which is what owning no target says.
  const std::optional<StaticStorageHome>& disable_target =
      process.Scopes().Get(b.scope).disable_target;

  for (const hir::StmtId child_hir_id : b.statements) {
    auto child_or = process.LowerStmt(hir_proc.stmts.Get(child_hir_id), inside);
    if (!child_or) return std::unexpected(std::move(child_or.error()));
    child_block.AppendStmt(*std::move(child_or));
  }
  end.emit(child_frame);
  // The extent is built where the block sits, not inside its body: a handler
  // it binds runs once the body has already been left.
  if (disable_target.has_value()) {
    return mir::Stmt{
        .label = std::move(label),
        .data = BuildTargetExtent(
            process, frame, std::move(child_block), *disable_target, landing)};
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
  // where it sits; one anywhere else is reached through the route to it.
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
            return process.RouteEnd(frame, t.target.id);
          }},
      d.target);
  const mir::ExprId services =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  // One call carries the whole statement (LRM 9.6.2): it invalidates the
  // target, wakes or ends what is inside it, and -- when the disabling
  // execution is itself inside the target -- leaves from here, which is what a
  // self-disable means. Nothing about where any affected execution lands is
  // decided here; the extent each entered says.
  const mir::ExprId disable = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = support::BuiltinFn::kDisable},
                  .arguments = {block.AsValue(unit.types, member), services}},
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
