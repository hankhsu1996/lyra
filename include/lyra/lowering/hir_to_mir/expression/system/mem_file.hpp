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

// LRM 21.4 / 21.5 memory file transfer. A void task appearing only in statement
// position, over an unpacked-array memory whose element lowers to a packed
// vector. `direction` selects the two shapes: a load (`$readmem{h,b}`) writes
// the memory, so it is an output argument and round-trips through a copy-out
// temp (LRM 13.5) -- copy-in initialized from the memory's current value so
// words the file does not address survive, filled by the runtime, then written
// back; a dump (`$writemem{h,b}`) reads the memory, so it lowers as a plain
// input read with no writeback. The declared bounds and the digit radix ride as
// ordinary operands.
auto LowerMemFileSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::MemFileSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
