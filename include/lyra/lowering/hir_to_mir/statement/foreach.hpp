#pragma once

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 12.7.3 `foreach`, as one ordinary loop per iterated dimension, nested
// outermost first. What a dimension's loop counts follows from the array's
// type there:
//
//   int a [2:5];        foreach (a[i])   i runs 2 through 5
//   int d [];           foreach (d[i])   i runs 0 up to a size read once, on
//                                        entry to the dimension
//   int m [string];     foreach (m[k])   k takes each stored key in order
//   int j [][];         foreach (j[r, c])  c's size is read from j[r], inside
//                                          the loop over r
//
// A `continue` in the body is the innermost loop's own, which advances to the
// next tuple of indices; a `break` leaves every loop, so it names the outermost
// one (LRM 12.8).
auto LowerForeachStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForeachStmt& f) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
