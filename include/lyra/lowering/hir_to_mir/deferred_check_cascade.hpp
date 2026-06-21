#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

struct DeferredCheckBranch {
  mir::ExprId predicate{};
  mir::Block body;
};

// Snapshots predicates into wrapper so the deferred closure body captures
// stable, side-effect-free values rather than re-evaluating the source
// expressions later. Caller stages wrapper with any prelude it needs
// (case puts its selector snapshot there; unique-if passes a fresh wrapper)
// and pre-lowers each body with the depth guards the cascade nesting requires.
auto BuildDeferredCheckCascade(
    ModuleLowerer& module, WalkFrame frame, mir::Block wrapper,
    std::vector<DeferredCheckBranch> branches,
    std::optional<mir::Block> tail_else, hir::UniquePriorityCheck check,
    std::optional<std::string> outer_label) -> mir::Stmt;

auto LowerUniqueIfStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::IfStmt& root) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
