#pragma once

// What `unique`, `unique0` and `priority` add to a conditional or case
// statement (LRM 12.4.2, 12.5.3): the claims a qualifier makes about the
// statement's arms, and the reports it owes where a run breaks one.

#include <optional>
#include <span>
#include <vector>

#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
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
// holds; what it claims about two arms holding at once is untouched. A
// statement with no qualifier claims nothing.
auto AssertionsOf(std::optional<hir::QualifiedCheck> check, bool has_catch_all)
    -> QualifiedAssertions;

// What a violation report calls the arms of the statement it is about: LRM
// 12.4.2 states its requirement over an if-else-if construct's conditions and
// 12.5.3 over a case statement's items.
enum class QualifiedArmKind { kCondition, kCaseItem };

// The arm a statement runs when none of its own held: the source's own `else`
// or `default` where it wrote one, and where it did not, the report a
// qualifier asserting totality owes -- arriving there is the violation. A
// statement never carries both, because an explicit catch-all is what
// discharges that assertion. Reporting is deferred as LRM 12.4.2.1 requires,
// and kept by the statement's assertion control (LRM 20.11).
auto BuildFallThrough(
    ProcessLowerer& process, const WalkFrame& frame,
    std::optional<mir::Block> catch_all,
    std::optional<hir::QualifiedCheck> check, QualifiedArmKind arm_kind)
    -> std::optional<mir::Block>;

// What a statement asserting uniqueness does before it selects an arm: `held`
// holds every arm's answer, already evaluated in `frame`'s block. Where more
// than one arm held, the statement keeps a violation report (LRM 12.4.2.1),
// and an execution with at most one keeps nothing.
void BuildUniquenessCheck(
    ProcessLowerer& process, const WalkFrame& frame,
    std::span<const mir::LocalId> held, const hir::QualifiedCheck& check,
    QualifiedArmKind arm_kind);

}  // namespace lyra::lowering::hir_to_mir
