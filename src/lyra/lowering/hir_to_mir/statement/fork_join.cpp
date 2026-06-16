#include "lyra/lowering/hir_to_mir/statement/fork_join.hpp"

#include <expected>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

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

}  // namespace

// LRM 9.3.2: a fork is a procedural scope. Its block_item_declarations lower
// into that scope and initialize at block entry -- in the parent, before any
// branch spawns. Each branch lowers to a closure composed into the fork
// scope's expr arena. The capture sink collects every enclosing reference
// the body makes as an identity; fork then assigns each capture's kind based
// on its declaration depth (LRM 6.21).
auto LowerForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForkStmt& f) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::ProceduralScope fork_scope;
  const WalkFrame fork_frame = frame.WithProceduralScope(&fork_scope).Deeper();
  const ProceduralDepth fork_depth = fork_frame.procedural_depth;

  for (const hir::StmtId local_hir_id : f.locals) {
    auto lowered =
        process.LowerStmt(hir_proc.stmts.at(local_hir_id.value), fork_frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    fork_scope.AppendStmt(*std::move(lowered));
  }

  std::vector<mir::ExprId> branch_ids;
  branch_ids.reserve(f.branches.size());
  for (const hir::StmtId branch_hir_id : f.branches) {
    const hir::Stmt& branch = hir_proc.stmts.at(branch_hir_id.value);
    mir::ProceduralScope branch_scope;

    CaptureSink sink{
        fork_frame.procedural_depth.Inner(), branch_scope, fork_scope};
    const WalkFrame branch_frame = fork_frame.WithClosure(&sink)
                                       .WithProceduralScope(&branch_scope)
                                       .Deeper();
    auto lowered = process.LowerStmt(branch, branch_frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    branch_scope.AppendStmt(*std::move(lowered));

    std::vector<mir::Capture> captures;
    for (const CaptureRequest& request : sink.TakeRequests()) {
      if (request.decl_depth == fork_depth) {
        captures.emplace_back(
            mir::ByValueCapture{
                .value = request.source, .binding = request.binding});
      } else {
        captures.emplace_back(
            mir::ByReferenceCapture{
                .target = request.source, .binding = request.binding});
      }
    }
    mir::ClosureExpr closure;
    closure.captures = std::move(captures);
    closure.body =
        std::make_unique<mir::ProceduralScope>(std::move(branch_scope));
    branch_ids.push_back(fork_scope.AddExpr(
        mir::Expr{
            .data = std::move(closure),
            .type = process.Module().Unit().builtins.void_type}));
  }

  const mir::ProceduralScopeId scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(fork_scope));
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ForkStmt{
          .mode = LowerJoinMode(f.mode),
          .scope = scope_id,
          .branches = std::move(branch_ids)}};
}

}  // namespace lyra::lowering::hir_to_mir
