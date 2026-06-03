#pragma once

#include <slang/ast/statements/LoopStatements.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

// Flattens the Cartesian product of the non-skipped dims into a single
// `hir::ForStmt` driven by a synthetic counter. The flat shape is what makes
// plain `BreakStmt` / `ContinueStmt` / `ReturnStmt` in the body satisfy
// LRM 12.8 directly -- see `docs/decisions/foreach-lowering.md`.
auto LowerForeachLoopStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::ForeachLoopStatement& fs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
