#include <cstddef>
#include <cstdint>
#include <optional>
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
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Constant = common::Constant;
using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerConcatenationExpression(
    const mir::ConcatenationExpression& concat, LirBuilder& builder)
    -> lir::TempRef {
  std::vector<TempRef> operand_temps;
  operand_temps.reserve(concat.operands.size());
  for (const auto& operand : concat.operands) {
    operand_temps.push_back(LowerExpression(*operand, builder));
  }
  auto result = builder.AllocateTemp("cat", concat.type);
  auto instruction =
      Instruction::Concatenation(result, std::move(operand_temps), concat.type);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerReplicationExpression(
    const mir::ReplicationExpression& rep, LirBuilder& builder)
    -> lir::TempRef {
  // Lower operand once (ensures single evaluation per LRM)
  auto operand_temp = LowerExpression(*rep.operand, builder);

  // Create vector with operand repeated count times
  std::vector<TempRef> operand_temps(rep.count, operand_temp);

  // Emit as concatenation instruction
  auto result = builder.AllocateTemp("rep", rep.type);
  auto instruction =
      Instruction::Concatenation(result, std::move(operand_temps), rep.type);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerNewArrayExpression(
    const mir::NewArrayExpression& new_arr, LirBuilder& builder)
    -> lir::TempRef {
  auto size = LowerExpression(*new_arr.size_expr, builder);
  auto result = builder.AllocateTemp("new_array", new_arr.type);

  std::optional<TempRef> init = std::nullopt;
  if (new_arr.init_expr) {
    init = LowerExpression(*new_arr.init_expr, builder);
  }
  builder.AddInstruction(
      Instruction::Allocate(
          result, new_arr.type, lir::StorageKind::kVector, size, init));
  return result;
}

auto LowerUnpackedStructLiteralExpression(
    const mir::UnpackedStructLiteralExpression& lit, LirBuilder& builder)
    -> lir::TempRef {
  // Create struct literal by storing field values into a new struct
  auto result = builder.AllocateTemp("struct_lit", lit.type);
  builder.AddInstruction(
      Instruction::Allocate(result, lit.type, lir::StorageKind::kVector));

  for (size_t i = 0; i < lit.field_values.size(); ++i) {
    auto field_value = LowerExpression(*lit.field_values[i], builder);

    // Emit constant for field index
    auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
    auto index_constant =
        builder.InternConstant(Constant::Int(static_cast<int32_t>(i)));
    builder.AddInstruction(
        Instruction::Basic(IK::kConstant, index_temp, index_constant));

    // Use unified StoreElement with temp operand
    builder.AddInstruction(
        Instruction::StoreElement(
            Operand::Temp(result), index_temp, field_value));
  }
  return result;
}

auto LowerArrayLiteralExpression(
    const mir::ArrayLiteralExpression& lit, LirBuilder& builder)
    -> lir::TempRef {
  // Create array/queue literal by building up values in SSA style
  auto current = builder.AllocateTemp("array_lit", lit.type);
  auto storage =
      lit.type.IsQueue() ? lir::StorageKind::kDeque : lir::StorageKind::kVector;
  builder.AddInstruction(Instruction::Allocate(current, lit.type, storage));

  // For queues, use push_back to add elements (SSA style: each returns new
  // queue) For dynamic arrays, use StoreElement (dynamic array starts with
  // size)
  bool is_queue = lit.type.IsQueue();

  for (size_t i = 0; i < lit.elements.size(); ++i) {
    auto element_value = LowerExpression(*lit.elements[i], builder);

    if (is_queue) {
      // SSA style: push_back returns the modified queue
      // %q1 = push_back(%q0, element) -> %q1 is new queue with element
      auto next = builder.AllocateTemp("array_lit", lit.type);
      void* push_back_fn = interpreter::ResolveIntrinsicMethod(
          common::Type::Kind::kQueue, "push_back");
      builder.AddInstruction(
          Instruction::IntrinsicCall(
              push_back_fn, current, {element_value}, next, lit.type));
      current = next;  // Chain: next iteration uses result of this one
    } else {
      // Use StoreElement for dynamic arrays
      auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
      auto index_constant =
          builder.InternConstant(Constant::Int(static_cast<int32_t>(i)));
      builder.AddInstruction(
          Instruction::Basic(IK::kConstant, index_temp, index_constant));
      builder.AddInstruction(
          Instruction::StoreElement(
              Operand::Temp(current), index_temp, element_value));
    }
  }
  return current;
}

}  // namespace lyra::lowering::mir_to_lir
