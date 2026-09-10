#pragma once

// Lowering of the procedural continuous assignments (LRM 10.6): `assign` and
// `deassign` on a variable, `force` and `release` on a variable or a net.

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// Puts the target under a takeover at the level the source's keyword named and
// starts the evaluation that keeps it current.
auto LowerProceduralContinuousAssignStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ProceduralContinuousAssignStmt& pca) -> diag::Result<mir::Stmt>;

// Takes the target back out of the takeover at that level. It reaches the
// evaluation through nothing: the target stops answering for the generation
// that evaluation carries, which is what tells it to stop.
auto LowerProceduralContinuousEndStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ProceduralContinuousEndStmt& pce) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
