#include "lyra/lowering/mir_to_lir/helpers.hpp"

#include <cstdint>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"

namespace lyra::lowering::mir_to_lir {

auto AdjustForNonZeroLower(
    lir::TempRef index, int32_t lower_bound, LirBuilder& builder)
    -> lir::TempRef {
  using lir::Instruction;
  using lir::Operand;
  using IK = lir::InstructionKind;

  if (lower_bound == 0) {
    return index;
  }
  auto offset_temp = builder.AllocateTemp("offset", common::Type::Int());
  auto offset_literal =
      builder.InternLiteral(common::Literal::Int(lower_bound));
  builder.AddInstruction(
      Instruction::Basic(IK::kLiteral, offset_temp, offset_literal));

  auto adjusted = builder.AllocateTemp("adj_idx", common::Type::Int());
  builder.AddInstruction(
      Instruction::Basic(
          IK::kBinarySubtract, adjusted,
          {Operand::Temp(index), Operand::Temp(offset_temp)}));
  return adjusted;
}

}  // namespace lyra::lowering::mir_to_lir
