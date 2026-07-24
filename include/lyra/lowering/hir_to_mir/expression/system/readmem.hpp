#pragma once

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 21.4 `$readmemh` / `$readmemb`. A void task that loads a memory from a
// named text file; it appears only in statement position. The output memory is
// an unpacked-array lvalue, so the call round-trips through a copy-out temp
// (LRM 13.5): the temp is copy-in initialized from the memory's current value
// so words the file does not address survive, the runtime ReadMem entry fills
// the temp, and the writeback commits it. The declared bounds and the digit
// radix ride as ordinary operands.
auto LowerReadMemSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::ReadMemSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
