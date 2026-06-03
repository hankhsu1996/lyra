#pragma once

#include <vector>

#include <slang/ast/Expression.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace slang::ast {
class ValueSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerProcExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr>;

auto LowerStructuralExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr>;

// Walks a slang assignment-target expression and rejects any form that is
// not addressable. Lvalue-ness is positional: assignment targets carry an
// ExprId pointing at any expression whose form is addressable. Allowed
// forms are a NamedValue referring to a Variable (not a generate-loop
// variable), ElementSelect / RangeSelect on an addressable base, and
// Concatenation of addressable operands (LRM 11.4.12 destructuring LHS).
//
// `proc_state` distinguishes process-body context (non-null, allows both
// structural-var and procedural-local targets) from continuous-assignment
// context (null, structural-var only; LRM 10.3).
auto ValidateAssignableSlangExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    const ProcessLoweringState* proc_state, const slang::ast::Expression& expr)
    -> diag::Result<void>;

// Lower one entry from a slang range_list (the operand list of `inside` and
// the per-item label list of `case (...) inside`). ValueRange entries become
// an InsideRangePair; any other expression becomes a plain ExprId. The
// resulting ExprId(s) are appended to proc_state's expression table.
auto LowerInsideItem(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& item_expr) -> diag::Result<hir::InsideItem>;

// Builds a reference expression to a leaf reached by navigating `path` from a
// local instance member (the head) down into a child unit. `target` is the
// leaf value symbol (the cross-unit dedup key); `member` and `home_frame`
// locate the head. Both a hierarchical reference (`c.x`, `m.l.x`) and a port
// connection's child-side endpoint resolve through this one path
// (reference_resolution.md).
auto MakeCrossUnitMemberRef(
    UnitLoweringState& unit_state, const slang::ast::ValueSymbol& target,
    ScopeFrameId home_frame, hir::InstanceMemberId member,
    std::vector<hir::PathStep> path, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr;

}  // namespace lyra::lowering::ast_to_hir
