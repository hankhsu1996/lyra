#include "lyra/lowering/hir_to_mir/statement/fork_join.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What a join mode dispatches to (LRM 9.3.2, Table 9-1): the entry that spawns
// the branches under that mode's wait condition, and whether the process that
// ran the fork waits for the condition at all -- every branch for `join`, the
// first of them for `join_any`, nothing for `join_none`. Both follow from the
// mode alone, so both are answered here rather than derived twice.
struct JoinDispatch {
  support::BuiltinFn callee;
  bool parent_waits;
};

auto DispatchForJoinMode(hir::JoinMode mode) -> JoinDispatch {
  switch (mode) {
    case hir::JoinMode::kAll:
      return {.callee = support::BuiltinFn::kForkWaitAll, .parent_waits = true};
    case hir::JoinMode::kAny:
      return {
          .callee = support::BuiltinFn::kForkWaitFirst, .parent_waits = true};
    case hir::JoinMode::kNone:
      return {.callee = support::BuiltinFn::kSpawnAll, .parent_waits = false};
  }
  throw InternalError("DispatchForJoinMode: unknown hir::JoinMode");
}

}  // namespace

// LRM 9.3.2: a fork is a block whose block_item_declarations lower into that
// block and initialize at block entry -- in the parent, before any branch
// spawns. The fork lowers as a plain `BlockStmt` (the block_item_declarations
// become ordinary `LocalDeclStmt`s) whose last statement is the mode's dispatch
// call. The branches cross as one machine array after the runtime handle, the
// way every run of same-typed operands crosses: a target that spells a call
// variadically expands the array, and one that names a length-and-address pair
// reads it as that pair, so neither spelling is stated here. Every mode's call
// yields nothing, so the modes differ only in whether the parent awaits it.
auto LowerForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForkStmt& f) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block fork_block;
  // A fork the source named is part of the hierarchical name of everything
  // lexically inside it (LRM 23.9 lists a fork-join block among the constructs
  // that define a scope), so entering it adopts its name node -- for the block
  // items, which run in this execution context, and for every branch, which
  // runs in its own.
  const DeclaredScope& fork_scope = process.Scopes().Get(f.scope);
  const WalkFrame fork_frame =
      frame.WithBlock(&fork_block)
          .WithScopeNameBorrowedHandle(fork_scope.NameBorrowedHandle());

  // A fork the source named is a target a `disable` can name (LRM 9.6.2), and
  // what it must end is every process executing the block -- the thread that
  // runs the block items and awaits the join, and each branch, which takes the
  // target's membership at the spawn. The region brackets the whole fork, so
  // the target is entered before any branch spawns and all of them are inside
  // it.
  const std::optional<mir::FieldId> cancel_target =
      process.BodyHasReceiver() ? fork_scope.cancellation_target : std::nullopt;

  // A branch snapshots the fork's own block-item declarations by value and
  // aliases any deeper-enclosing variable it reads (LRM 6.21 / 9.3.2). The
  // policy names those block-item origins as the snapshot set; every other
  // forwarded origin aliases.
  CapturePolicy branch_policy;
  for (const hir::StmtId local_hir_id : f.locals) {
    const auto* vd =
        std::get_if<hir::VarDeclStmt>(&hir_proc.stmts.Get(local_hir_id).data);
    if (vd != nullptr) {
      branch_policy.snapshot_set.insert(BindingOriginId::Procedural(vd->var));
    }
  }

  for (const hir::StmtId local_hir_id : f.locals) {
    auto lowered =
        process.LowerStmt(hir_proc.stmts.Get(local_hir_id), fork_frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    fork_block.AppendStmt(*std::move(lowered));
  }

  const auto& builtins = process.Owner().Unit().builtins;
  const mir::ExprId runtime_id =
      fork_block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));

  std::vector<mir::ExprId> branches;
  branches.reserve(f.branches.size());
  for (const hir::StmtId branch_hir_id : f.branches) {
    const hir::Stmt& branch = hir_proc.stmts.Get(branch_hir_id);
    // LRM 9.3.2: a branch is a concurrent thread whose body may suspend on
    // timing controls / event waits, so it lowers as a coroutine closure --
    // returns inside it become `co_return`. The branch policy snapshots the
    // fork's own block-item declarations and aliases deeper enclosing
    // variables (LRM 6.21).
    ClosureBuilder closure(process.Owner().Unit(), fork_frame, branch_policy);
    auto lowered = process.LowerStmt(branch, closure.Frame());
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    closure.Body().AppendStmt(*std::move(lowered));
    branches.push_back(fork_block.exprs.Add(closure.BuildCoroutine()));
  }

  const std::size_t branch_count = branches.size();
  const mir::ExprId branches_id = fork_block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(branches)},
          .type = mir::MachineArrayOf(
              process.Owner().Unit().types, builtins.coroutine_void,
              branch_count)});

  const JoinDispatch dispatch = DispatchForJoinMode(f.mode);
  const mir::ExprId call_id = fork_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = dispatch.callee},
                  .arguments = {runtime_id, branches_id}},
          .type = builtins.void_type});

  const mir::ExprId stmt_expr_id =
      dispatch.parent_waits
          ? fork_block.exprs.Add(
                mir::Expr{
                    .data = mir::AwaitExpr{.awaitable = call_id},
                    .type = builtins.void_type})
          : call_id;
  fork_block.AppendStmt(mir::ExprStmt{.expr = stmt_expr_id});

  // The region that consumes the effect sits where the fork sits, not inside
  // it: an execution the `disable` reached has already left the fork by the
  // time the handler runs, and it resumes after it (LRM 9.6.2).
  if (cancel_target.has_value()) {
    return mir::Stmt{
        .label = std::move(label),
        .data = BuildCancellableRegion(
            process, frame, std::move(fork_block), *cancel_target)};
  }
  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(fork_block));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

// LRM 9.6.1 `wait fork`: suspend the executing process until its immediate
// children have terminated. It lowers to a single awaited runtime call taking
// only the runtime handle; the child set is resolved at runtime from the
// executing process, so MIR carries no operand. The awaited call's result type
// is `void`, the same await shape as `join`.
auto LowerWaitForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  const auto& builtins = process.Owner().Unit().builtins;
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kWaitFork},
                  .arguments = {runtime_id}},
          .type = builtins.void_type});
  const mir::ExprId await_id = block.exprs.Add(
      mir::Expr{
          .data = mir::AwaitExpr{.awaitable = call_id},
          .type = builtins.void_type});
  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = await_id}};
}

// LRM 9.6.3 `disable fork` terminates every descendant of the executing
// process. It lowers to a single runtime call taking only the runtime handle;
// the descendant set is resolved at runtime, so MIR carries no operand. The
// caller does not block, so the call is not awaited -- the same shape as
// `join_none`.
auto LowerDisableForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  const auto& builtins = process.Owner().Unit().builtins;
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kDisableFork},
                  .arguments = {runtime_id}},
          .type = builtins.void_type});
  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = call_id}};
}

}  // namespace lyra::lowering::hir_to_mir
