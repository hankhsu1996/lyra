#pragma once

#include <optional>
#include <span>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// The memory a task names to load or dump by its indices.
[[nodiscard]] auto NameMemory(
    ProcessLowerer& process, const WalkFrame& frame, const hir::Expr& memory)
    -> diag::Result<NamedMemory>;

// The memory a load writes, as the target of that write, its steps settled for
// the checks the write owes (LRM 11.9).
[[nodiscard]] auto MemoryWriteTarget(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path,
    const hir::Expr& memory) -> Lvalue;

// The indices of a fixed-size memory's unpacked dimensions `dims`, highest
// first, as the operand an entry addressing it by them takes -- each
// dimension's left index then its right, as machine integers -- and the lowest
// index of the highest dimension, where a run starts when the source names no
// start. A slice's highest dimension is the indices it holds.
struct MemoryIndices {
  mir::ExprId ranges;
  mir::ExprId lowest;
};

[[nodiscard]] auto MemoryIndicesOf(
    mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const hir::UnpackedRange> dims,
    const std::optional<SliceIndices>& slice) -> MemoryIndices;

// LRM 21.4 / 21.5 memory file transfer. A void task appearing only in statement
// position, over an unpacked-array memory whose element lowers to a packed
// vector. `direction` selects the two shapes: a load (`$readmem{h,b}`) writes
// the memory, so it is an output argument and round-trips through a copy-out
// temp (LRM 13.5) -- copy-in initialized from the memory's current value so
// words the file does not address survive, filled by the runtime, then written
// back; a dump (`$writemem{h,b}`) reads the memory, so it lowers as a plain
// input read with no writeback. The memory's indices and the digit radix ride
// as ordinary operands.
auto LowerMemFileSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::MemFileSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
