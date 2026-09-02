#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// What a qualified statement asserts about its arms, once an explicit catch-all
// has discharged what it covers. LRM 12.4.2 and 12.5.3 give a qualifier two
// independent claims, and each decides both what a violation report says and
// how much of the statement has to run to decide it.
struct QualifiedAssertions {
  // At most one arm holds. Deciding it needs every arm's answer, which is why
  // the standard has a `unique` or `unique0` statement go on evaluating and
  // comparing past the arm it selects.
  bool uniqueness;
  // Some arm holds. Deciding it needs only whether control reached the arm that
  // runs when none of the others did.
  bool totality;
};

// LRM 12.4.2 and 12.5.3. An explicit `else` or `default` covers every value the
// arms left, so a statement carrying one claims nothing about whether some arm
// holds; what it claims about two arms holding at once is untouched.
auto AssertionsOf(hir::UniquePriorityCheck check, bool has_catch_all)
    -> QualifiedAssertions;

// What a violation report calls the arms of the statement it is about: LRM
// 12.4.2 states its requirement over an if-else-if construct's conditions and
// 12.5.3 over a case statement's items.
enum class QualifiedArmKind { kCondition, kCaseItem };

// The if-else-if series a qualifier applies to. LRM 12.4.2 has the keyword
// govern the entire series rather than the one `if` that carries it, so both
// the arms it checks and the `else` that discharges its totality assertion are
// read off the series -- an `else if` continues it and is not that `else`.
struct QualifiedIfSeries {
  std::vector<const hir::IfStmt*> arms;
  std::optional<hir::StmtId> else_arm;
};

auto SeriesOf(const hir::ProceduralBody& proc, const hir::IfStmt& root)
    -> QualifiedIfSeries;

// The arm a statement asserting totality runs when none of its own matched:
// arriving there is the violation, so this reports it and carries nothing else.
// Reporting is deferred to the Observed region as LRM 12.4.2.1 requires, which
// is why the arm submits rather than emits, and the submitted body reads
// nothing the statement computed.
auto BuildTotalityReportScope(
    UnitLowerer& unit_lowerer, WalkFrame frame, hir::UniquePriorityCheck check,
    QualifiedArmKind arm_kind, diag::SourceSpan span) -> mir::Block;

// The arm a series runs when none of its conditions held: the source's own
// `else` where it wrote one, and the report totality owes where it did not. A
// series never carries both, because an explicit `else` is what discharges that
// assertion (LRM 12.4.2).
auto LowerIfFallThrough(
    ProcessLowerer& process, WalkFrame frame, const QualifiedIfSeries& series,
    hir::UniquePriorityCheck check, diag::SourceSpan span)
    -> diag::Result<std::optional<mir::Block>>;

struct QualifiedArm {
  mir::ExprId predicate;
  mir::Block body;
};

// Assembles a statement that asserts uniqueness: every arm's predicate is
// evaluated and snapshotted up front, the cascade then dispatches on those
// snapshots, and a body submitted to the Observed region counts them and
// reports where more than one held. `wrapper` arrives staged with whatever
// prelude the statement needs -- a case puts its selector snapshot there -- and
// each arm's body is pre-lowered. `fall_through` is the arm reached when no
// predicate held, which is the source's own catch-all where it wrote one and
// the report totality owes where it did not.
auto BuildUniquenessCheckCascade(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::Block wrapper,
    std::vector<QualifiedArm> arms, std::optional<mir::Block> fall_through,
    hir::UniquePriorityCheck check, QualifiedArmKind arm_kind,
    std::optional<std::string> outer_label, diag::SourceSpan span) -> mir::Stmt;

auto LowerUniquenessIfSeries(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const QualifiedIfSeries& series, hir::UniquePriorityCheck check,
    diag::SourceSpan span) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
