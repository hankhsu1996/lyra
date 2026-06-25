#include "lyra/lowering/hir_to_mir/statement/fork_join.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
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

// Whether a branch closure aliases an enclosing automatic cell. LRM 6.21
// requires the scope enclosing a fork-join to keep its automatic storage alive
// for every spawned branch; Lyra does not yet extend a non-persistent (task or
// fork-branch) activation frame to cover a detached branch, so a branch that
// aliases one of its locals would dangle once that frame is gone. The alias is
// a `RefType` environment binding (LRM 6.21); a static / module variable
// instead reaches the branch through `self` (the receiver binding, a pointer,
// never a `RefType`), so persistent storage never matches.
auto AliasesEnclosingAutomatic(
    const mir::CompilationUnit& unit, const mir::Expr& branch) -> bool {
  const auto& closure = std::get<mir::ClosureExpr>(branch.data);
  for (const mir::EnvBinding& binding : closure.environment) {
    const mir::TypeId binding_type =
        closure.code->body.vars.Get(binding.param).type;
    if (std::holds_alternative<mir::RefType>(unit.GetType(binding_type).data)) {
      return true;
    }
  }
  return false;
}

}  // namespace

// LRM 9.3.2: a fork is a block whose block_item_declarations lower into that
// block and initialize at block entry -- in the parent, before any branch
// spawns. The fork lowers as a plain `BlockStmt` (the block_item_declarations
// become ordinary `LocalDeclStmt`s) whose last statement is the dispatch call:
// `ForkWaitAll` / `ForkWaitFirst` (awaited) for `join` / `join_any`, or
// `SpawnAll` (no await) for `join_none`. The runtime entry is variadic over
// branches; MIR carries them as ordinary call arguments after the services
// handle. The result type is `void` for every dispatch -- the awaitable's
// `await_resume` is void, and `SpawnAll` returns void directly -- so the
// `AwaitExpr` / `ExprStmt` shape is the same regardless of mode.
auto LowerForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForkStmt& f) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block fork_block;
  const WalkFrame fork_frame = frame.WithBlock(&fork_block).Deeper();
  const BlockDepth fork_depth = fork_frame.block_depth;

  for (const hir::StmtId local_hir_id : f.locals) {
    auto lowered =
        process.LowerStmt(hir_proc.stmts.Get(local_hir_id), fork_frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    fork_block.AppendStmt(*std::move(lowered));
  }

  const auto& builtins = process.Module().Unit().builtins;
  const mir::ExprId services_id =
      fork_block.exprs.Add(BuildServicesCallExpr(process.Module(), fork_frame));

  std::vector<mir::ExprId> call_args;
  call_args.reserve(1 + f.branches.size());
  call_args.push_back(services_id);
  for (const hir::StmtId branch_hir_id : f.branches) {
    const hir::Stmt& branch = hir_proc.stmts.Get(branch_hir_id);
    // LRM 9.3.2: a branch is a concurrent thread whose body may suspend on
    // timing controls / event waits, so it lowers as a coroutine closure --
    // returns inside it become `co_return`. A fork-scope local is snapshotted
    // by value (the `fork_depth` boundary); a deeper enclosing variable aliases
    // the live cell through a reference capture (LRM 6.21).
    ClosureBuilder closure(
        process.Module().Unit(), fork_frame, true, fork_depth);
    auto lowered = process.LowerStmt(branch, closure.Frame());
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    closure.Body().AppendStmt(*std::move(lowered));
    mir::Expr branch_closure = closure.BuildCoroutine();
    if (AliasesEnclosingAutomatic(process.Module().Unit(), branch_closure)) {
      return diag::Fail(
          branch.span, diag::DiagCode::kUnsupportedForkJoinForm,
          "a fork-join branch by-reference capturing an enclosing automatic "
          "variable is not yet supported (LRM 6.21)");
    }
    call_args.push_back(fork_block.exprs.Add(std::move(branch_closure)));
  }

  const support::BuiltinFn callee_id = JoinModeToCallee(f.mode);
  const mir::ExprId call_id = fork_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::FreeFnCallee{.id = callee_id},
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
  fork_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = stmt_expr_id}});

  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(fork_block));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
