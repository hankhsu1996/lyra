#include "lyra/lowering/hir_to_mir/pattern.hpp"

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
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

    // Lower the fill expression
    Result<mir::Operand> fill_result =
        LowerExpression(fill_data.fill_expr, builder);
    if (!fill_result) return std::unexpected(fill_result.error());

    mir::Operand fill_operand;

    if (fill_data.is_bit_fill) {
      // Unbased-unsized literal: use 1-bit fill value directly
      // The fill value is already 1-bit; replicate it across all bits
      mir::PlaceId fill_temp = ctx.AllocTemp(ctx.GetBitType());
      builder.EmitAssign(fill_temp, *fill_result);
      fill_operand = mir::Operand::Use(fill_temp);
    } else {
      // Sized literal: context-typed conversion to element type
      const mir::Place& target_place = (*ctx.mir_arena)[target];
      TypeId target_type_id = mir::TypeOfPlace(*ctx.type_arena, target_place);
      const Type& target_type = (*ctx.type_arena)[target_type_id];

      TypeId element_type_id;
      if (target_type.Kind() == TypeKind::kIntegral) {
        // Integral: element is 1-bit
        element_type_id = ctx.GetBitType();
      } else if (target_type.Kind() == TypeKind::kPackedArray) {
        // Packed array: use array's element type
        const auto& arr_info = target_type.AsPackedArray();
        element_type_id = arr_info.element_type;
      } else {
        throw common::InternalError(
            "LowerPattern",
            "fill pattern target must be integral or packed array");
      }

      mir::PlaceId fill_temp = ctx.AllocTemp(element_type_id);
      builder.EmitAssign(fill_temp, *fill_result);
      fill_operand = mir::Operand::Use(fill_temp);
    }

    // Emit FillPacked effect operation
    builder.EmitEffect(
        mir::FillPackedEffect{.target = target, .fill_value = fill_operand});

    return {};
  }

  // Unknown pattern kind
  throw common::InternalError("LowerPattern", "unknown pattern kind");
}

}  // namespace lyra::lowering::hir_to_mir
