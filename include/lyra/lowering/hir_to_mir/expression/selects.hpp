#pragma once

// LRM 11.5 select expressions: element-, range-, and member-select.
// LRM 7.2.1: a packed struct / union field access lowers as a slice over
// the aggregate's bit plane -- MIR carries no struct-specific node.

#include <cstddef>
#include <cstdint>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Reading a run of a packed value's vector by position, for a consumer that
// has no source-level select to lower: pattern matching (LRM 12.6)
// destructures a value the source named only as a whole. Both entries produce
// the read side only; a write reaches a member through the member-access
// lowering below.

// The `width` bits starting at `bit_offset`, as an owned value of
// `result_type`. Unguarded: the caller states which bits it wants.
[[nodiscard]] auto BuildPackedRunRead(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    std::uint64_t bit_offset, std::uint64_t bit_width, mir::TypeId result_type)
    -> mir::Expr;

// Member `index` of the aggregate `projection` describes, at `result_type`.
// Reaching a tagged union's member this way is checked against the tag (LRM
// 11.9); the check is part of the produced expression.
[[nodiscard]] auto BuildPackedMemberRead(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    const PackedProjection& projection, std::size_t index,
    mir::TypeId result_type) -> mir::Expr;

// The one-bit test that the tag `base` currently carries names member `index`
// (LRM 7.3.2 places the tag at the most significant bits). Bit-pattern
// equality, not a logical compare: a tag carrying x or z names no member, and
// answering that definitely rather than unknown is what lets a guarded access
// or a pattern arm sit behind the test.
[[nodiscard]] auto BuildPackedTagTest(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    const PackedProjection& projection, std::size_t index) -> mir::ExprId;

// A select's meaning is independent of the enclosing scope, so one template
// over the pass class serves both the procedural and structural contexts;
// explicit instantiations live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerHirElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirClassPropertyAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ClassPropertyAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// LHS-context selector lowerings: like the read-context handlers but the
// base lowers through `LowerLhsExpr`, leaving the chain cell-rooted with no
// `ObservableMethod{kGet}` wrap.
template <ExprLowerer Lowerer>
auto LowerHirElementSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirClassPropertyAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ClassPropertyAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
