#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/sampled_history.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// The class field holding one history, as the place every operation on it acts
// through. It needs no route: a history is recorded on the scope whose process
// reads it, so the storage and the reader are always the same scope (LRM
// 16.9.3).
[[nodiscard]] auto BuildSampledHistoryExpr(
    mir::Block& block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer, hir::SampledHistoryId id)
    -> mir::ExprId;

// The process that keeps one history current. LRM 14.7 already describes the
// sampling agent as an always procedure -- one copy per instance of the scope
// declaring it -- and a history has to record every tick of its clock whether
// or not anything is reading it, so it is lowered as one.
[[nodiscard]] auto LowerSampledHistorySampler(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    hir::SampledHistoryId id, const hir::SampledHistoryDecl& history)
    -> diag::Result<mir::CallableDecl>;

// `$past`: the value the tick it names settled, and nothing else (LRM 16.9.3).
template <ExprLowerer Lowerer>
[[nodiscard]] auto LowerPastValueCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::PastValueRef& ref)
    -> diag::Result<mir::Expr>;

// A value change function: the sampled value of this time step -- the one
// `$sampled` gives, read here the same way -- against the one the most recent
// strictly prior tick settled, because the standard defines a change as one
// against the other (LRM 16.9.3).
template <ExprLowerer Lowerer>
[[nodiscard]] auto LowerValueChangeCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const hir::ValueChangeRef& ref, mir::TypeId result_type,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
