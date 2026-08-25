#include "lyra/lowering/hir_to_mir/expression/tagged_union.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/flat_packed_type.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 7.3.2 fixes a packed tagged union's layout: the tag at the most
// significant bits, the member's own bits at the least significant ones, and
// whatever lies between them undefined. Building the value is therefore the
// concatenation of those three runs, each carrying what that region holds --
// the undefined run carries the union's own default, which is what "undefined"
// resolves to for a value of this state domain. A run of zero width contributes
// nothing: a member as wide as the union leaves no undefined run, a `void`
// member contributes no bits of its own, and a union declaring a single member
// needs no tag to tell its members apart.
template <ExprLowerer Lowerer>
auto BuildPackedTaggedValue(
    Lowerer& lowerer, WalkFrame frame, const PackedProjection& layout,
    const hir::TaggedUnionExpr& t, std::optional<mir::ExprId> payload,
    mir::TypeId result_type) -> mir::Expr {
  auto& owner = lowerer.Owner();
  auto& unit = owner.Unit();
  auto& block = *frame.current_block;
  const auto member_width = layout.members[t.member_index.value].bit_width;
  const auto gap_width = layout.bit_width - layout.tag_bits - member_width;

  // Every run is carried in the union's own state domain, so the runs compose
  // into exactly the union's vector and only its signedness is left to
  // reconcile.
  const mir::BitAtom atom = unit.types.Get(result_type).AsPackedArray().atom;
  std::vector<mir::ExprId> runs;
  if (layout.tag_bits > 0) {
    const mir::ExprId named = block.exprs.Add(
        mir::MakeIntLiteral(
            unit.builtins.int_type,
            static_cast<std::int64_t>(t.member_index.value)));
    runs.push_back(ConvertToType(
        unit, block, named, InternFlatPacked(unit, layout.tag_bits, atom)));
  }
  if (gap_width > 0) {
    runs.push_back(block.exprs.Add(BuildDefaultValueExpr(
        owner, frame, InternFlatPacked(unit, gap_width, atom))));
  }
  if (payload.has_value()) {
    runs.push_back(ConvertToType(
        unit, block, *payload, InternFlatPacked(unit, member_width, atom)));
  }

  const mir::ExprId concat = block.exprs.Add(
      mir::Expr{
          .data = mir::ConcatExpr{.operands = std::move(runs)},
          .type = InternFlatPacked(unit, layout.bit_width, atom)});
  return BuildValueConversion(unit, block, concat, result_type);
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerHirTaggedUnionExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::TaggedUnionExpr& t,
    hir::TypeId union_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const hir::Type& hir_type = lowerer.Owner().Hir().types.Get(union_type);

  std::optional<mir::ExprId> payload;
  if (t.payload.has_value()) {
    auto payload_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*t.payload), frame);
    if (!payload_or) return std::unexpected(std::move(payload_or.error()));
    payload = block.exprs.Add(*std::move(payload_or));
  }

  if (std::holds_alternative<hir::PackedUnionType>(hir_type.data)) {
    return BuildPackedTaggedValue(
        lowerer, frame, ProjectPackedAggregate(lowerer.Owner(), hir_type.data),
        t, payload, result_type);
  }

  // LRM 11.9: `tagged Member` without an operand names a `void` member, whose
  // component type has one value. Supplying that value here is what lets the
  // MIR primitive carry a payload for every tag, so nothing downstream has to
  // answer for an absent one.
  if (!payload.has_value()) {
    const mir::TypeId component = TaggedComponentType(
        lowerer.Owner().Unit(), result_type, t.member_index);
    payload = block.exprs.Add(
        BuildDefaultValueExpr(lowerer.Owner(), frame, component));
  }
  return mir::Expr{
      .data = mir::TaggedExpr{.tag_index = t.member_index, .payload = *payload},
      .type = result_type};
}

template auto LowerHirTaggedUnionExpr(
    ProcessLowerer&, WalkFrame, const hir::TaggedUnionExpr&, hir::TypeId,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirTaggedUnionExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::TaggedUnionExpr&,
    hir::TypeId, mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
