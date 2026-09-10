#pragma once

// Lowering of simple immediate assertion statements: `assert` / `assume` and
// `cover` without a timing qualifier (LRM 16.3), and the two things every form
// of the family shares: what a false result reports when the source wrote no
// fail statement, and whether an arm carries anything to run.

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// With no fail statement, what a false result selects is the tool's own report,
// which the standard fixes at error severity (LRM 16.3, 16.14.1). A pass
// statement does not take its place. The emit goes into `block`, which is a
// fresh scope for a simple assertion, the deferred closure body for a deferred
// one, and the fail action for a concurrent one.
void AppendDefaultReport(
    ProcessLowerer& process, mir::Block& block,
    hir::AssertionDirective directive, diag::SourceSpan span);

// Whether an action arm carries a statement to run. An omitted arm is absent,
// and slang gives an omitted arm an empty statement rather than none, so both
// stand for "no action here" (LRM 16.3, 16.4).
[[nodiscard]] auto HasRealArm(
    const hir::ProceduralBody& body, std::optional<hir::StmtId> arm) -> bool;

auto LowerAssertStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::AssertStmt& a, diag::SourceSpan span) -> diag::Result<mir::Stmt>;

auto LowerCoverStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CoverStmt& c, diag::SourceSpan span) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
