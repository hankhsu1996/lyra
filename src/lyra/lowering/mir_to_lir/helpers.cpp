#include "lyra/lowering/mir_to_lir/helpers.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"

namespace lyra::lowering::mir_to_lir {

using common::Constant;
using common::Type;
using lir::Instruction;
using lir::Operand;
using IK = lir::InstructionKind;

auto AdjustForNonZeroLower(
    lir::TempRef index, int32_t lower_bound, LirBuilder& builder)
    -> lir::TempRef {
  if (lower_bound == 0) {
    return index;
  }
  auto offset_temp = builder.AllocateTemp("offset", Type::Int());
  auto offset_constant = builder.InternConstant(Constant::Int(lower_bound));
  builder.AddInstruction(
      Instruction::Basic(IK::kConstant, offset_temp, offset_constant));

  auto adjusted = builder.AllocateTemp("adj_idx", Type::Int());
  builder.AddInstruction(
      Instruction::Basic(
          IK::kBinarySubtract, adjusted,
          {Operand::Temp(index), Operand::Temp(offset_temp)}));
  return adjusted;
}

auto GetElementWidthAfterIndices(
    const common::Type& base_type, size_t num_indices) -> size_t {
  const Type* current = &base_type;
  for (size_t i = 0; i < num_indices; ++i) {
    current = &current->GetElementType();
  }
  return current->GetBitWidth();
}

auto ComputeCompositeIndex(
    const std::vector<std::unique_ptr<mir::Expression>>& indices,
    const common::Type& base_type, LirBuilder& builder) -> lir::TempRef {
  // Single index - just lower it directly
  if (indices.size() == 1) {
    return LowerExpression(*indices[0], builder);
  }

  // Multiple indices - compute composite: ((idx0 * count1 + idx1) * count2 ...)
  auto composite = LowerExpression(*indices[0], builder);
  const Type* current_type = &base_type;

  for (size_t i = 1; i < indices.size(); ++i) {
    const auto& inner_type = current_type->GetElementType();
    auto inner_count = static_cast<int32_t>(inner_type.GetElementCount());

    // composite = composite * inner_count + indices[i]
    auto count_temp = builder.AllocateTemp("cnt", Type::Int());
    auto count_constant = builder.InternConstant(Constant::Int(inner_count));
    builder.AddInstruction(
        Instruction::Basic(IK::kConstant, count_temp, count_constant));

    auto mul_temp = builder.AllocateTemp("mul", Type::Int());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryMultiply, mul_temp,
            {Operand::Temp(composite), Operand::Temp(count_temp)}));

    auto idx_temp = LowerExpression(*indices[i], builder);
    auto add_temp = builder.AllocateTemp("add", Type::Int());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryAdd, add_temp,
            {Operand::Temp(mul_temp), Operand::Temp(idx_temp)}));

    composite = add_temp;
    current_type = &inner_type;
  }

  return composite;
}

}  // namespace lyra::lowering::mir_to_lir
