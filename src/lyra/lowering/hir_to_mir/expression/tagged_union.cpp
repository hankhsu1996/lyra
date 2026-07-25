#include "lyra/lowering/hir_to_mir/expression/tagged_union.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto LowerHirTaggedUnionExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::TaggedUnionExpr& t,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;

  // LRM 11.9: `tagged Member` without an operand names a `void` member, whose
  // component type has one value. Supplying that value here is what lets the
  // MIR primitive carry a payload for every tag, so nothing downstream has to
  // answer for an absent one.
  mir::ExprId payload{};
  if (t.payload.has_value()) {
    auto payload_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*t.payload), frame);
    if (!payload_or) return std::unexpected(std::move(payload_or.error()));
    payload = block.exprs.Add(*std::move(payload_or));
  } else {
    const mir::TypeId component = TaggedComponentType(
        lowerer.Owner().Unit(), result_type, t.member_index);
    payload = block.exprs.Add(
        BuildDefaultValueExpr(lowerer.Owner(), frame, component));
  }
  return mir::Expr{
      .data = mir::TaggedExpr{.tag_index = t.member_index, .payload = payload},
      .type = result_type};
}

template auto LowerHirTaggedUnionExpr(
    ProcessLowerer&, WalkFrame, const hir::TaggedUnionExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirTaggedUnionExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::TaggedUnionExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
