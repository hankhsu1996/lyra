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
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Maps a fork-join join mode to the runtime entry that realizes its wait
// behavior. `kAll` resumes after every branch finishes; `kAny` after the first;
// `kNone` returns void so the parent never awaits at all.
auto JoinModeToCallee(hir::JoinMode mode) -> support::BuiltinFn {
  switch (mode) {
    case hir::JoinMode::kAll:
      return support::BuiltinFn::kForkWaitAll;
    case hir::JoinMode::kAny:
      return support::BuiltinFn::kForkWaitFirst;
    case hir::JoinMode::kNone:
      return support::BuiltinFn::kSpawnAll;
  }
  throw InternalError("JoinModeToCallee: unknown hir::JoinMode");
}

}  // namespace

// LRM 9.3.2: a fork is a block whose block_item_declarations lower into that
// block and initialize at block entry -- in the parent, before any branch
// spawns. The fork lowers as a plain `BlockStmt` (the block_item_declarations
// become ordinary `LocalDeclStmt`s) whose last statement is the dispatch call:
// `ForkWaitAll` / `ForkWaitFirst` (awaited) for `join` / `join_any`, or
// `SpawnAll` (no await) for `join_none`. The runtime entry is variadic over
// branches; MIR carries them as ordinary call arguments after the runtime
// handle. The result type is `void` for every dispatch -- the awaitable's
// `await_resume` is void, and `SpawnAll` returns void directly -- so the
// `AwaitExpr` / `ExprStmt` shape is the same regardless of mode.
auto LowerForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForkStmt& f) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block fork_block;
  const WalkFrame fork_frame = frame.WithBlock(&fork_block);

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

  std::vector<mir::ExprId> call_args;
  call_args.reserve(1 + f.branches.size());
  call_args.push_back(runtime_id);
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
    call_args.push_back(fork_block.exprs.Add(closure.BuildCoroutine()));
  }

  const support::BuiltinFn callee_id = JoinModeToCallee(f.mode);
  const mir::ExprId call_id = fork_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = callee_id},
                  .arguments = std::move(call_args)},
          .type = builtins.void_type});

  // `join_none` returns void; `join` / `join_any` return a runtime
  // `JoinAwaitable` whose `await_resume` is void -- either way the call's
  // result type is `void` and the dispatch shape only differs in whether the
  // parent suspends.
  const mir::ExprId stmt_expr_id =
      f.mode == hir::JoinMode::kNone
          ? call_id
          : fork_block.exprs.Add(
                mir::Expr{
                    .data = mir::AwaitExpr{.awaitable = call_id},
                    .type = builtins.void_type});
  fork_block.AppendStmt(mir::ExprStmt{.expr = stmt_expr_id});

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
