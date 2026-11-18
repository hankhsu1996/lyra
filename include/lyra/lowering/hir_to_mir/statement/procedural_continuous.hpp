#pragma once

// Lowering of the procedural continuous assignments (LRM 10.6): `assign` and
// `deassign` on a variable, `force` and `release` on a variable or a net.

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/takeover_level.hpp"

namespace lyra::lowering::hir_to_mir {

// The takeover level `level` as the machine integer a takeover's entries take.
[[nodiscard]] auto LevelOperand(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::TakeoverLevel level) -> mir::ExprId;

// One place of a takeover's target as the takeover acts on it: what holds the
// place, and which positions of what it holds the place is, as a pair of
// machine integers: the first and how many (LRM 10.6.2).
struct TakenPart {
  mir::ExprId holder;
  mir::ExprId first;
  mir::ExprId count;
};

// The positions of its holder the place `place` names, which every takeover --
// a procedural one, and one foreign code asks for (LRM 38.34) -- takes over.
// LRM 10.6.1 admits only a whole singular variable and 10.6.2 adds a net and a
// constant part-select of one, and a caller may ask for a whole unpacked
// variable or any part of a variable as well, run with the rules 10.6.2 gives
// a whole variable applied to each position, numbered as the holder's type
// numbers them. A part of a value of a dynamic type and a member of a tagged
// union lie at no position of a fixed numbering, and are refused at `span`.
// Each select's position is read more than once, so `place` is one settled
// where it was named.
[[nodiscard]] auto TakeoverPart(
    mir::CompilationUnit& unit, const WalkFrame& frame, const AccessPath& place,
    diag::SourceSpan span) -> diag::Result<TakenPart>;

// What a takeover of `place` is driven with to show `value` there: a value of
// the type its holder holds, of which the takeover reads the positions it
// holds. Where the place is part of that value, `value` is written at the part
// in a value of it built in `frame`'s block for the purpose, whose other
// positions nothing reads.
[[nodiscard]] auto DrivenValue(
    mir::CompilationUnit& unit, const WalkFrame& frame, const AccessPath& place,
    mir::ExprId value) -> mir::ExprId;

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
