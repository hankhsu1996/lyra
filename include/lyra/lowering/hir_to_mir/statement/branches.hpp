#pragma once

// Lowering of branching statements: `if` / `unique if` / `unique0 if` /
// `priority if` (LRM 12.4); `case` / `casez` / `casex` / `unique case` /
// `unique0 case` / `priority case` (LRM 12.5); and `case ... inside`
// (LRM 12.5.4).

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerIfStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::IfStmt& i) -> diag::Result<mir::Stmt>;

auto LowerCaseStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseStmt& c) -> diag::Result<mir::Stmt>;

auto LowerCaseInsideStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseInsideStmt& c) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
