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
  // Memory-form: Allocate -> ResolveField -> Store -> Load
  // This enforces that all mutation happens through addresses.

  // Create pointer type for struct
  const auto* struct_type = builder.GetContext()->InternType(lit.type);
  auto ptr_type = common::Type::Pointer(struct_type);

  // Allocate storage, get pointer
  auto ptr = builder.AllocateTemp("struct_ptr", ptr_type);
  builder.AddInstruction(
      Instruction::Allocate(ptr, ptr_type, lir::StorageKind::kVector));

  // Store each field through pointer
  const auto& struct_data = std::get<common::UnpackedStructData>(lit.type.data);
  for (size_t i = 0; i < lit.field_values.size(); ++i) {
    auto field_value = LowerExpression(*lit.field_values[i], builder);

    // Get field type and create pointer to field
    const auto* field_type =
        builder.GetContext()->InternType(*struct_data.fields[i].field_type);
    auto field_ptr_type = common::Type::Pointer(field_type);
    auto field_ptr = builder.AllocateTemp("field_ptr", field_ptr_type);

    builder.AddInstruction(Instruction::ResolveField(field_ptr, ptr, i));
    builder.AddInstruction(Instruction::Store(field_ptr, field_value));
  }

  // Load final value from pointer
  auto result = builder.AllocateTemp("struct_lit", lit.type);
  builder.AddInstruction(Instruction::Load(result, ptr));
  return result;
}

auto LowerArrayLiteralExpression(
    const mir::ArrayLiteralExpression& lit, LirBuilder& builder)
    -> lir::TempRef {
  // For queues, use SSA-style push_back (already correct, no mutation)
  // For arrays, use memory-form: Allocate -> ResolveIndex -> Store -> Load
  bool is_queue = lit.type.IsQueue();

  if (is_queue) {
    // SSA-style: push_back returns new queue, chain them
    auto current = builder.AllocateTemp("queue_lit", lit.type);
    builder.AddInstruction(
        Instruction::Allocate(current, lit.type, lir::StorageKind::kDeque));

    for (size_t i = 0; i < lit.elements.size(); ++i) {
      auto element_value = LowerExpression(*lit.elements[i], builder);

      auto next = builder.AllocateTemp("queue_lit", lit.type);
      void* push_back_fn = interpreter::ResolveIntrinsicMethod(
          common::Type::Kind::kQueue, "push_back");
      builder.AddInstruction(
          Instruction::IntrinsicCall(
              push_back_fn, current, {element_value}, next, lit.type));
      current = next;
    }
    return current;
  }

  // Memory-form for arrays: Allocate -> ResolveIndex -> Store -> Load
  const auto* array_type = builder.GetContext()->InternType(lit.type);
  auto ptr_type = common::Type::Pointer(array_type);

  // Allocate storage, get pointer
  auto ptr = builder.AllocateTemp("array_ptr", ptr_type);
  builder.AddInstruction(
      Instruction::Allocate(ptr, ptr_type, lir::StorageKind::kVector));

  // Get element type
  const auto& elem_type = lit.type.GetElementType();
  const auto* elem_type_ptr = builder.GetContext()->InternType(elem_type);
  auto elem_ptr_type = common::Type::Pointer(elem_type_ptr);

  // Store each element through pointer
  for (size_t i = 0; i < lit.elements.size(); ++i) {
    auto element_value = LowerExpression(*lit.elements[i], builder);

    // Create index constant
    auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
    auto index_constant =
        builder.InternConstant(Constant::Int(static_cast<int32_t>(i)));
    builder.AddInstruction(Instruction::Constant(index_temp, index_constant));

    // Resolve element pointer and store
    auto elem_ptr = builder.AllocateTemp("elem_ptr", elem_ptr_type);
    builder.AddInstruction(
        Instruction::ResolveIndex(elem_ptr, ptr, index_temp));
    builder.AddInstruction(Instruction::Store(elem_ptr, element_value));
  }

  // Load final value from pointer
  auto result = builder.AllocateTemp("array_lit", lit.type);
  builder.AddInstruction(Instruction::Load(result, ptr));
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
