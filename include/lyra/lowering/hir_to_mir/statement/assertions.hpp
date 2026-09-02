#pragma once

// Lowering of simple immediate assertion statements: `assert` / `assume` and
// `cover` without a timing qualifier (LRM 16.3).

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerAssertStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::AssertStmt& a, diag::SourceSpan span) -> diag::Result<mir::Stmt>;

auto LowerCoverStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CoverStmt& c, diag::SourceSpan span) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
