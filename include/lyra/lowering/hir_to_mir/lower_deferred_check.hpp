#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

struct DeferredCheckBranch {
  mir::ExprId predicate;
  mir::ProceduralScope body;
};

// Snapshots predicates into wrapper_state so the deferred closure body captures
// stable, side-effect-free values rather than re-evaluating the source
// expressions later. Caller stages wrapper_state with any prelude it needs
// (case puts its selector snapshot there; unique-if passes a fresh wrapper)
// and pre-lowers each body with the depth guards the cascade nesting requires.
auto BuildDeferredCheckCascade(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState wrapper_state,
    std::vector<DeferredCheckBranch> branches,
    std::optional<mir::ProceduralScope> tail_else,
    hir::UniquePriorityCheck check, std::optional<std::string> outer_label)
    -> mir::Stmt;

auto LowerUniqueIfStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    const hir::Stmt& stmt, const hir::IfStmt& root) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
