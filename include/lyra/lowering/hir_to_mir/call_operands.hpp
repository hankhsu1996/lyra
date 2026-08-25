#pragma once

#include <cstddef>
#include <optional>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"

// Where a call's elided-slot vocabulary ends.
//
// HIR records an argument list as SV wrote it, so a position left empty --
// `$fread(mem, fd, , count)`, LRM 21.3.4.4 form 2d -- is `std::nullopt` at that
// slot. MIR has no such notion: `mir::CallExpr` takes a plain operand list.
// Translating that away is HIR-to-MIR's job, and doing it anywhere but the top
// of a lowering leaves every later line restating the same claim, once per
// read, with nothing checking that they agree.
//
// So a lowering opens by naming the operand shape its subroutine has. What it
// gets back is operands, not slots, and nothing downstream sees an optional
// again. Reaching a hole means the shape named here disagrees with the one
// slang admitted the call against, which is a compiler bug.
namespace lyra::lowering::hir_to_mir {

// Every operand of a call whose arity is whatever the user wrote: a user call,
// or a variadic system subroutine that elides nothing.
[[nodiscard]] auto RequiredOperands(const hir::CallExpr& call)
    -> std::vector<hir::ExprId>;

// A fixed-arity subroutine: exactly `count` operands, all present.
[[nodiscard]] auto RequiredOperands(
    const hir::CallExpr& call, std::size_t count) -> std::vector<hir::ExprId>;

// The fixed head of a variadic subroutine -- $fdisplay's descriptor, $sformat's
// output variable. What follows is the lowering's own to read.
[[nodiscard]] auto RequiredLeadingOperands(
    const hir::CallExpr& call, std::size_t count) -> std::vector<hir::ExprId>;

// A position whose elision this lowering honours. Absent covers both ways SV
// leaves one out: a hole in the middle and a short argument list.
[[nodiscard]] auto OptionalOperand(const hir::CallExpr& call, std::size_t index)
    -> std::optional<hir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
