#include "lyra/lowering/hir_to_mir/pattern.hpp"

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerPattern(
    hir::PatternId pattern_id, mir::PlaceId target, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();
  const auto& pattern = (*ctx.hir_arena)[pattern_id];

  if (pattern.kind == hir::PatternKind::kFill) {
    const auto& fill_data = std::get<hir::FillPatternData>(pattern.data);

    // Get target type and compute fill shape (destination-driven)
    const mir::Place& target_place = (*ctx.mir_arena)[target];
    TypeId target_type_id = mir::TypeOfPlace(*ctx.type_arena, target_place);
    const Type& target_type = (*ctx.type_arena)[target_type_id];

    if (!IsPackedIntegralLike(target_type_id, *ctx.type_arena)) {
      throw common::InternalError(
          "LowerPattern",
          "fill pattern target must be integral or packed array");
    }

    // Get bit_type for kIntegral case (use 4-state nature from target)
    bool is_four_state = IsPackedFourState(target_type, *ctx.type_arena);
    TypeId bit_type = ctx.GetUnitBitType(is_four_state);
    PackedFillShape shape =
        ComputePackedFillShape(target_type_id, *ctx.type_arena, bit_type);

    // unit_type is always valid now
    TypeId unit_type_id = shape.unit_type;

    // Lower the fill expression
    Result<mir::Operand> fill_result =
        LowerExpression(fill_data.fill_expr, builder);
    if (!fill_result) return std::unexpected(fill_result.error());

    // Check if fill expression type matches unit_type_id
    // If not, emit a cast
    const hir::Expression& fill_expr = (*ctx.hir_arena)[fill_data.fill_expr];
    mir::Operand fill_operand = *fill_result;

    if (fill_expr.type != unit_type_id) {
      // Emit cast to unit type
      mir::PlaceId cast_temp = ctx.AllocTemp(unit_type_id);
      mir::Rvalue cast_rvalue{
          .operands = {fill_operand},
          .info = mir::CastRvalueInfo{
              .source_type = fill_expr.type, .target_type = unit_type_id}};
      builder.EmitAssign(cast_temp, std::move(cast_rvalue));
      fill_operand = mir::Operand::Use(cast_temp);
    } else {
      // Materialize to temp for operand
      mir::PlaceId fill_temp = ctx.AllocTemp(unit_type_id);
      builder.EmitAssign(fill_temp, fill_operand);
      fill_operand = mir::Operand::Use(fill_temp);
    }

    // Emit FillPacked effect with full semantic payload
    builder.EmitEffect(
        mir::FillPackedEffect{
            .target = target,
            .fill_value = fill_operand,
            .unit_bits = shape.unit_bits,
            .count = static_cast<uint32_t>(shape.count),
            .total_bits = shape.total_bits});

    return {};
  }

  // Unknown pattern kind
  throw common::InternalError("LowerPattern", "unknown pattern kind");
}

}  // namespace lyra::lowering::hir_to_mir
