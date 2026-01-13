#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/interpreter/intrinsic.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/expression/internal.hpp"
#include "lyra/lowering/mir_to_lir/helpers.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Constant = common::Constant;
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
    auto width_constant = builder.InternConstant(
        Constant::Int(static_cast<int32_t>(element_width)));
    auto width_temp = builder.AllocateTemp("width", Type::Int());
    builder.AddInstruction(Instruction::Constant(width_temp, width_constant));
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryMultiply, bit_offset,
            {Operand::Temp(adjusted_index), Operand::Temp(width_temp)}));

    auto instruction =
        Instruction::LoadPackedBits(result, value, bit_offset, select.type);
    builder.AddInstruction(std::move(instruction));
    return result;
  }

  // Unpacked array/queue element access - use intrinsic
  const auto& value_type = select.value->type;
  void* intrinsic_fn = interpreter::ResolveIntrinsicIndexRead(value_type.kind);

  // Compute lower_bound for unpacked arrays (queues always have 0)
  int32_t lower_bound = 0;
  if (value_type.kind == Type::Kind::kUnpackedArray) {
    const auto& array_data =
        std::get<common::UnpackedArrayData>(value_type.data);
    lower_bound = array_data.lower_bound;
  }

  TempRef receiver;
  if (select.value->kind == mir::Expression::Kind::kIdentifier) {
    // Direct variable access: arr[i]
    const auto& array_id = mir::As<mir::IdentifierExpression>(*select.value);
    const auto* pointee = builder.GetContext()->InternType(value_type);
    auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
    builder.AddInstruction(Instruction::ResolveVar(ptr, array_id.symbol));
    receiver = builder.AllocateTemp("recv", value_type);
    builder.AddInstruction(Instruction::Load(receiver, ptr));
  } else {
    // Nested access (e.g., arr[i][j]): recursively lower the array expression
    receiver = LowerExpression(*select.value, builder);
  }

  // Emit kIntrinsicCall with receiver and index as args
  auto instr = Instruction::IntrinsicCall(
      intrinsic_fn, receiver, {index}, result, select.type);
  instr.lower_bound = lower_bound;
  builder.AddInstruction(std::move(instr));
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

  // Create a constant for the LSB shift amount
  auto lsb_temp = builder.AllocateTemp("lsb", Type::Int());
  auto lsb_constant = builder.InternConstant(Constant::Int(lsb));
  builder.AddInstruction(Instruction::Constant(lsb_temp, lsb_constant));

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
    auto offset_constant =
        builder.InternConstant(Constant::Int(indexed.width - 1));
    builder.AddInstruction(Instruction::Constant(offset_temp, offset_constant));

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
    auto receiver = LowerExpression(*member.value, builder);
    auto result = builder.AllocateTemp("field", member.type);

    // For struct: bit_offset is reused as field_index
    // For union: ALWAYS use index 0 (shared storage)
    size_t storage_index =
        member.value->type.IsUnpackedUnion() ? 0 : member.bit_offset;

    // Emit constant for field index
    auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
    auto index_constant = builder.InternConstant(
        Constant::Int(static_cast<int32_t>(storage_index)));
    builder.AddInstruction(Instruction::Constant(index_temp, index_constant));

    // Resolve intrinsic for field access
    void* intrinsic_fn =
        interpreter::ResolveIntrinsicIndexRead(member.value->type.kind);
    builder.AddInstruction(
        Instruction::IntrinsicCall(
            intrinsic_fn, receiver, {index_temp}, result, member.type));
    return result;
  }

  // Packed struct: use bit extraction
  auto value = LowerExpression(*member.value, builder);

  // Create a constant for the bit offset (LSB position)
  auto offset_temp = builder.AllocateTemp("offset", Type::Int());
  auto offset_constant = builder.InternConstant(
      Constant::Int(static_cast<int32_t>(member.bit_offset)));
  builder.AddInstruction(Instruction::Constant(offset_temp, offset_constant));

  // Use LoadPackedBits to extract the field bits
  auto result = builder.AllocateTemp("field", member.type);
  builder.AddInstruction(
      Instruction::LoadPackedBits(result, value, offset_temp, member.type));
  return result;
}

auto LowerHierarchicalReferenceExpression(
    const mir::HierarchicalReferenceExpression& hier_ref, LirBuilder& builder)
    -> lir::TempRef {
  // Hierarchical reference uses target_symbol directly (flat storage model)
  const auto* pointee = builder.GetContext()->InternType(hier_ref.type);
  auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
  builder.AddInstruction(Instruction::ResolveVar(ptr, hier_ref.target_symbol));

  auto result = builder.AllocateTemp("hier", hier_ref.type);
  builder.AddInstruction(Instruction::Load(result, ptr));
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
