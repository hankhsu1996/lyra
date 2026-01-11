#include <algorithm>
#include <cassert>
#include <cstdint>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/expression.hpp"
#include "lyra/lowering/mir_to_lir/expression_internal.hpp"
#include "lyra/lowering/mir_to_lir/helpers.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Literal = common::Literal;
using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerElementSelectExpression(
    const mir::ElementSelectExpression& select, LirBuilder& builder)
    -> lir::TempRef {
  assert(select.value);
  assert(select.selector);

  auto index = LowerExpression(*select.selector, builder);
  auto result = builder.AllocateTemp("elem", select.type);

  // Check if this is bitvector type (value) or unpacked array (variable)
  if (select.value->type.IsBitvector()) {
    // Packed vector: lower the value, then select element/bit
    auto value = LowerExpression(*select.value, builder);
    int32_t lower = select.value->type.GetElementLower();
    auto adjusted_index = AdjustForNonZeroLower(index, lower, builder);

    // Compute bit_offset = adjusted_index * element_width
    size_t element_width = select.type.GetBitWidth();
    auto bit_offset = builder.AllocateTemp("bit_offset", Type::Int());
    auto width_literal = builder.InternLiteral(
        Literal::Int(static_cast<int32_t>(element_width)));
    auto width_temp = builder.AllocateTemp("width", Type::Int());
    builder.AddInstruction(
        Instruction::Basic(IK::kLiteral, width_temp, width_literal));
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryMultiply, bit_offset,
            {Operand::Temp(adjusted_index), Operand::Temp(width_temp)}));

    auto instruction =
        Instruction::LoadPackedBits(result, value, bit_offset, select.type);
    builder.AddInstruction(std::move(instruction));
    return result;
  }

  // Unpacked array element access
  if (select.value->kind == mir::Expression::Kind::kIdentifier) {
    // Direct variable access: arr[i]
    const auto& array_id = mir::As<mir::IdentifierExpression>(*select.value);
    auto instruction = Instruction::LoadElement(
        result, Operand::Variable(array_id.symbol), index, select.type);
    builder.AddInstruction(std::move(instruction));
    return result;
  }

  // Nested access (e.g., arr[i][j]): recursively lower the array expression
  auto array_temp = LowerExpression(*select.value, builder);
  auto instruction = Instruction::LoadElement(
      result, Operand::Temp(array_temp), index, select.type);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerRangeSelectExpression(
    const mir::RangeSelectExpression& range, LirBuilder& builder)
    -> lir::TempRef {
  assert(range.value);

  auto value = LowerExpression(*range.value, builder);

  // Compute LSB: for [7:4], LSB is 4
  int32_t lsb = std::min(range.left, range.right);

  // Adjust for non-zero-based ranges (e.g., bit [63:32])
  // Packed structs return 0 from GetElementLower() (always 0-based)
  if (range.value->type.IsBitvector()) {
    lsb -= range.value->type.GetElementLower();
  }

  // Create a literal for the LSB shift amount
  auto lsb_temp = builder.AllocateTemp("lsb", Type::Int());
  auto lsb_literal = builder.InternLiteral(Literal::Int(lsb));
  auto lsb_instruction =
      Instruction::Basic(IK::kLiteral, lsb_temp, lsb_literal);
  builder.AddInstruction(std::move(lsb_instruction));

  auto result = builder.AllocateTemp("slice", range.type);
  auto instruction =
      Instruction::LoadPackedBits(result, value, lsb_temp, range.type);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerIndexedRangeSelectExpression(
    const mir::IndexedRangeSelectExpression& indexed, LirBuilder& builder)
    -> lir::TempRef {
  assert(indexed.value);
  assert(indexed.start);

  auto value = LowerExpression(*indexed.value, builder);
  auto start = LowerExpression(*indexed.start, builder);

  TempRef lsb_temp;
  if (indexed.is_ascending) {
    // a[i+:4]: lsb = i (start index is the LSB)
    lsb_temp = start;
  } else {
    // a[i-:4]: lsb = i - width + 1
    auto offset_temp = builder.AllocateTemp("offset", Type::Int());
    auto offset_literal =
        builder.InternLiteral(Literal::Int(indexed.width - 1));
    builder.AddInstruction(
        Instruction::Basic(IK::kLiteral, offset_temp, offset_literal));

    lsb_temp = builder.AllocateTemp("lsb", Type::Int());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinarySubtract, lsb_temp,
            {Operand::Temp(start), Operand::Temp(offset_temp)}));
  }

  // Adjust for non-zero-based ranges if needed
  int32_t lower = indexed.value->type.GetElementLower();
  lsb_temp = AdjustForNonZeroLower(lsb_temp, lower, builder);

  auto result = builder.AllocateTemp("slice", indexed.type);
  builder.AddInstruction(
      Instruction::LoadPackedBits(result, value, lsb_temp, indexed.type));
  return result;
}

auto LowerMemberAccessExpression(
    const mir::MemberAccessExpression& member, LirBuilder& builder)
    -> lir::TempRef {
  assert(member.value);

  // Check if this is an unpacked struct/union access
  if (member.value->type.IsUnpackedStruct() ||
      member.value->type.IsUnpackedUnion()) {
    auto value = LowerExpression(*member.value, builder);
    auto result = builder.AllocateTemp("field", member.type);

    // For struct: bit_offset is reused as field_index
    // For union: ALWAYS use index 0 (shared storage)
    size_t storage_index =
        member.value->type.IsUnpackedUnion() ? 0 : member.bit_offset;

    // Emit literal for storage index
    auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
    auto index_literal = builder.InternLiteral(
        Literal::Int(static_cast<int32_t>(storage_index)));
    builder.AddInstruction(
        Instruction::Basic(IK::kLiteral, index_temp, index_literal));

    // Use unified LoadElement with temp operand
    builder.AddInstruction(
        Instruction::LoadElement(
            result, Operand::Temp(value), index_temp, member.type));
    return result;
  }

  // Packed struct: use bit extraction
  auto value = LowerExpression(*member.value, builder);

  // Create a literal for the bit offset (LSB position)
  auto offset_temp = builder.AllocateTemp("offset", Type::Int());
  auto offset_literal = builder.InternLiteral(
      Literal::Int(static_cast<int32_t>(member.bit_offset)));
  builder.AddInstruction(
      Instruction::Basic(IK::kLiteral, offset_temp, offset_literal));

  // Use LoadPackedBits to extract the field bits
  auto result = builder.AllocateTemp("field", member.type);
  builder.AddInstruction(
      Instruction::LoadPackedBits(result, value, offset_temp, member.type));
  return result;
}

auto LowerHierarchicalReferenceExpression(
    const mir::HierarchicalReferenceExpression& hier_ref, LirBuilder& builder)
    -> lir::TempRef {
  auto result = builder.AllocateTemp("hier", hier_ref.type);
  auto instruction = Instruction::LoadHierarchical(
      result, hier_ref.instance_path, hier_ref.target_symbol, hier_ref.type);
  builder.AddInstruction(std::move(instruction));
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
