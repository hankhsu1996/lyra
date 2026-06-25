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
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerJoinMode(hir::JoinMode mode) -> mir::JoinMode {
  switch (mode) {
    case hir::JoinMode::kAll:
      return mir::JoinMode::kAll;
    case hir::JoinMode::kAny:
      return mir::JoinMode::kAny;
    case hir::JoinMode::kNone:
      return mir::JoinMode::kNone;
  }
  throw InternalError("LowerJoinMode: unknown hir::JoinMode");
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
// spawns. Each branch lowers to a coroutine closure composed into the fork
// block's expr arena.
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

  std::vector<mir::ExprId> branches;
  branches.reserve(f.branches.size());
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
    branches.push_back(fork_block.exprs.Add(std::move(branch_closure)));
  }

  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(fork_block));
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ForkStmt{
          .mode = LowerJoinMode(f.mode),
          .scope = scope_id,
          .branches = std::move(branches)}};
}

}  // namespace lyra::lowering::hir_to_mir
